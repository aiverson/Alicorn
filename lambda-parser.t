local absal = require 'absal'
_ENV = nil --hack lulpeg into working
local lpeg = require 'LuLPeg.lulpeg'

local M = {}

function M.lambda(param, body)
  return function(ctx)
    print "entering lambda creation"
    local fun = ctx.net:new_node(1)
    print("lambda fun", fun.val)
    local era = ctx.net:new_node(0)
    print("lambda era", era.val)
    ctx.net:link(absal.node_port(fun, 1), absal.node_port(era, 0))
    print("first link")
    M.dump_net(ctx.net)
    ctx.net:link(absal.node_port(era, 1), absal.node_port(era, 2))
    print("second link")
    M.dump_net(ctx.net)
    local shadow = ctx.scope[arg]
    ctx.scope[param] = {fun = fun, kind = ctx.net:new_dup_type()}
    ctx.varcount = ctx.varcount + 1 -- note: unique variable index could be used as key to store debug info
    print("lambda before body")
    local bodyport = body(ctx)
    print("lambda after body", bodyport.val)
    ctx.net:link(absal.node_port(fun, 2), bodyport)
    ctx.scope[param] = shadow
    M.dump_net(ctx.net)
    return absal.node_port(fun, 0)
  end
end

function M.call(fun, arg)
  return function(ctx)
    print("before call node creation")
    local node = ctx.net:new_node(1)
    print("expr node", node.val)
    local fun_port = fun(ctx)
    print ("fun_port ", fun_port)
    ctx.net:link(absal.node_port(node, 0), fun_port)
    print "function linked"
    ctx.net:link(absal.node_port(node, 1), arg(ctx))
    print("expr links")
    M.dump_net(ctx.net)
    print("expr port", absal.node_port(node, 2).val)
    return absal.node_port(node, 2)
  end
end

function M.variable(name)
  return function(ctx)
    print("inside variable")
    local argdat = ctx.scope[name]
    print("arg fun", argdat.fun.val)
    local arg = ctx.net:enter(absal.node_port(argdat.fun, 1))
    print "in variable after arg"
    if ctx.net:kind(absal.port_to_node(arg)) == 0 then
      print "in variable after then"
      ctx.net:reuse_node(absal.port_to_node(arg))
      print("in variable after reuse node", absal.port_to_node(arg).val)
      M.dump_net(ctx.net)
      return absal.node_port(argdat.fun, 1)
    else
      print "in variable after else"
      local dup = ctx.net:new_node(argdat.kind)
      print "in variable after dup"
      ctx.net:link(absal.node_port(dup, 2), arg)
      ctx.net:link(absal.node_port(dup, 0), absal.node_port(argdat.fun, 1))
      M.dump_net(ctx.net)
      return absal.node_port(dup, 1)
    end
  end
end

local symbol_idx = 0
function M.symbol(name)
  name = name or ""
  symbol_idx = symbol_idx + 1
  return name.."$"..symbol_idx
end

local grammar
do
  local P, S, R, V = lpeg.P, lpeg.S, lpeg.R, lpeg.V
  local C, Cf, Carg = lpeg.C, lpeg.Cf, lpeg.Carg
  local id = C((R"az"+R"AZ")^1)
  local basic_space = (S" \t\n" + P"--"*(P(1) - "\n"))
  local space = basic_space^1
  local optspace = basic_space^0
  local function list(pat, sep)
    return pat * (sep * pat)^0
  end
  grammar = P {
      V"expr",
      expr = Cf(list(V"subexpr", space),
                function(fun, arg)
                  print("expr")
                  return M.call(fun, arg)
      end),
      subexpr = P"(" * optspace * V"expr" * optspace * P")"
        + V"letin"
        + V"lambda"
        + V"variable",
      letin = P"let" * space * id * space * P"=" * space * V"expr" * space * P"in" * V"expr" / function(name, val, expr)
        return M.call(M.lambda(name, expr), val)
                                                                                               end,
      lambda = P"\\" * optspace * id * space * P"->" * space * V"expr" / function(arg, bod)
        print("lambda", arg, bod)
        return M.lambda(arg, bod)
                                                                        end,
      variable = id / function(name)
        print("variable", name)
        return M.variable(name)
                      end
  }
end

function M.parse_lambda(src)
  return grammar:match(src)
end

function M.lambda_to_net(lambda)
  local ctx = {
    net = absal.new_net(),
    scope = {},
    varcount = 2
  }
  M.dump_net(ctx.net)
  local port = lambda(ctx)
  ctx.net:link(absal.node_port(absal.new_node_ref(0), 0), port)
  return ctx.net
end

function M.dump_net(net)
  print("net with "..net.node_count.." nodes")
  for i = 0, net.node_count - 1 do
    local node = net.nodes[i]
    if node.kind >= 0 then
      print ("node "..i.. ":", node.internal.ports[0].val, node.internal.ports[1].val, node.internal.ports[2].val, node.kind)
    else
      print("node "..i..":", node.external.port.val, "0x"..bit.tohex(node.external.value._intptr), node.kind)
    end
  end
end

function M.net_to_string(net)
  local node_depth = {}
  local output_buffer = {}
  local function emit(str) table.insert(output_buffer, str) end
  local function varname(id)
    if id == 0 then return "_" end
    local letters = {}
    while id > 0 do
      table.insert(letters, string.char(id % 26 + 97))
      id = math.floor(id / 26)
    end
    return table.concat(letters)
  end
  local function go(next, exit, depth)
    local prev = net:enter(next)
    local prev_slot = absal.port_to_slot(prev)
    local prev_node = absal.port_to_node(prev)
    if net:kind(prev_node) == 1 then
      if prev_slot == 0 then
        node_depth[prev_node.val] = depth
        emit("\\ ")
        emit(varname(net:kind(absal.port_to_node(net:enter(absal.node_port(prev_node, 1))))))
        emit " -> "
        return go(absal.node_port(prev_node, 2), exit, depth + 1)
      elseif prev_slot == 1 then
        emit(varname(net:kind(absal.port_to_node(net:enter(absal.node_port(prev_node, 1))))))
      elseif prev_slot == 2 then
        go(absal.node_port(prev_node, 0), exit, depth)
        return go(absal.node_port(prev_node, 1), exit, depth)
      else error "invalid slot"
      end
    else
      local wire = absal.port(prev_node, prev_port > 0 and 0 or exit.head)
      local port = prev_port > 0 and {head = prev_port, tail = exit} or exit.tail
      return go(wire, port, depth)
    end
  end
  go(absal.node_port(absal.new_node_ref(0), 0), nil, 0)
  return table.concat(output_buffer)
end

return M
