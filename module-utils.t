local absal = require 'absal'

local M = {}

local function M.synthesize_duplicate(inner)
  return terra(net: &absal.net, x: absal.node_ref, y: absal.node_ref)
    var a = net:enter(absal.node_port(y, 1))
    var b = net:enter(absal.node_port(y, 2))
    var z = net:new_node(net:kind(x))
    inner(&z.value, &x.value)
    net:link(absal.node_port(x, 0), a)
    net:link(absal.node_port(z, 0), b)
    net:reuse_node(y)
  end
end

local function M.synthesize_erase(inner)
  return terra(net: &absal.net, x: absal.node_ref, y: absal.node_ref)
    inner(&x.value)
    net:reuse_node(x)
    net:reuse_node(y)
  end
end

function M.define_value_type(ctx, type, duplicate, erase)
  local type_idx = ctx.net:new_type()
  local info = ctx.net.typeinfo[type_idx]
  info.category = 4
  info.methods[0] = M.synthesize_erase(erase):getpointer()
  info.methods[1] = absal.crash:getpointer()
  info.methods[2] = M.synthesize_duplicate(duplicate):getpointer()
  info.methods[3] = absal.crash:getpointer()
  info.methods[4] = absal.crash:getpointer()
  ctx.types[type] = type_idx
  return type_idx
end

local terra linked_erase(net: &absal.net, x: absal.node_ref, y: absal.node_ref)
  net:link(absal.node_port(y, 0), net:enter(absal.node_port(x, 1)))
  net:link(net:new_erase_node(), net:enter(absal.node_port(x, 2)))
  net:reuse_node(x)
end

function M.synthesize_arg_to_close(next_tidx)
  return terra(net: &absal.net, x: absal.node_ref, y: absal.node_ref)
    var a = net:new_node(next_tidx)
    [
      terralib.newlist{{0, 1}, {1, 0}, {2, 2}} :map(function(ports)
          return `net:link(absal.node_port(a, [ports[1] ]), net:enter(absal.node_port(x, [ports[2] ])))
                                                   end)
    ]
    net:reuse_node(x)
  end
end

function M.define_function_transition(ctx, next_state, eager)
  if eager then
    local type_idx = ctx.net:new_type()
    local info = ctx.net.typeinfo[type_idx]
    info.category = 3
    info.methods[0] = linked_erase:getpointer()
    info.methods[1] = absal.crash:getpointer()
    info.methods[2] = absal.crash:getpointer()
    info.methods[3] = absal.crash:getpointer()
    info.methods[4] = M.synthesize_arg_to_close(next_state):getpointer()
    --TODO: finish
  end
  --TODO: finish
end

function M.register_function(ctx, fn)
  local fun_type = fn:gettype()
  local has_types = true
  for _, v in ipairs(fun_type.parameters) do
    has_types = has_types and ctx.types[v] and true
  end
  if fun_type.returntype.convertible == "tuple" then
    for _, v in ipairs(fun_type.returntype:getentries()) do
      has_types = has_types and ctx.types[v]
    end
  else
    has_types = has_types and ctx.types[fun_type.returntype]
  end
  assert(has_types, "unable to register function with undefined types")
  --TODO: finish
end
