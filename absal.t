
local malloc = terralib.externfunction("malloc", intptr -> &opaque)
local free = terralib.externfunction("free", &opaque -> {})
local realloc_raw = terralib.externfunction("realloc", {&opaque, intptr} -> &opaque)
local realloc = macro(function(ptr, size) return `[ptr:gettype()](realloc_raw([&opaque](ptr), size * [terralib.sizeof(ptr:gettype().type)])) end)

local M = {}

local function Stack(typ)
  local struct StackImpl {
    base: &typ
    idx: intptr
    capacity: intptr
                         }
  terra StackImpl:top()
    return self.base[self.idx]
  end

  local terra checkgrow(s: &StackImpl)
    if s.idx == s.capacity then
      s.capacity = s.capacity * 2
      s.base = realloc(s.base, s.capacity)
    end
  end

  terra StackImpl:push(val: typ)
    self.idx = self.idx + 1
    checkgrow(self)
    self.base[self.idx] = val
  end

  terra StackImpl:pop()
    self.idx = self.idx - 1
    return self.base[self.idx + 1]
  end

  terra StackImpl:empty()
    return self.idx == 0
  end

  terra StackImpl:init()
    self.base = [&typ](malloc([terralib.sizeof(typ)]))
    self.idx = 0
    self.capacity = 1
  end

  terra StackImpl:destroy()
    free(self.base)
  end

  function StackImpl.metamethods:__for(body)
    return quote
      while not self:empty() do
        [body(`self:pop())]
      end
           end
  end
  return StackImpl
end
Stack = terralib.memoize(Stack)

local struct node_ref { val: int }
local struct port_ref { val: int }
local slot_ref = int
terra M.new_node_ref(val: int) return [node_ref]{val} end
M.node_ref = node_ref
M.port_ref = port_ref
M.slot_ref = slot_ref

function node_ref.metamethods:__tostring() return "node_ref("..self.val..")" end
function port_ref.metamethods:__tostring() return "port_ref(" .. M.port_to_node(self).val .. ", " .. M.port_to_slot(self) .. ")" end

node_ref.metamethods.__eq = macro(function(a, b) return `[a].val == [b].val end,
  function(a, b) return a.val == b.val end)
node_ref.metamethods.__ne = macro(function(a, b) return `[a].val ~= [b].val end,
  function(a, b) return a.val ~= b.val end)

struct M.externval {
  union {
    _int: int
    _ptr: &opaque
    _intptr: intptr
    _double: double
    _float: float
  }
                      }

local struct external_shape {
  port: port_ref
  value: M.externval
                           }

local struct internal_shape {
  ports: port_ref[3]
                           }

local struct node {
  union {
    internal: internal_shape
    external: external_shape
  }
  kind: int
                  }

local node_extra_entries = {
  port = function(self) return `self.external.port end,
  ports = function(self) return `self.internal.ports end,
  value = function(self) return `self.external.value end
}
node.metamethods.__entrymissing = macro(function(name, object)
  if node_extra_entries[name] then
    return node_extra_entries[name](object)
  end
end)

local empty_val = constant(`[node_ref]{-1})

local n_node_categories = 5

struct M.net {
  nodes: &node
  node_count: int
  node_cap: int
  unused_next: node_ref
  typeinfo: &(struct {
                methods: ({&M.net, node_ref, node_ref} -> {})[n_node_categories]
                role: int
             })
  type_count: int
  type_cap: int
             }

terra M.port_to_node(val: port_ref): node_ref
  return [node_ref]{val.val >> 2}
end

terra M.port_to_slot(val: port_ref): slot_ref
  return val.val and 3
end

terra M.node_port(n: node_ref, s: slot_ref): port_ref
  return [port_ref]{n.val << 2 or s}
end

local terra clear_node(n: &node)
  n.ports[0].val = -1
  n.ports[1].val = -1
  n.ports[2].val = -1
end

  --DEBUG
local C = terralib.includec"stdio.h"

terra M.net:new_node(kind: int)
  C.printf("start of new_node\n")
  if self.unused_next ~= empty_val then
    var res = self.unused_next
    self.unused_next = M.port_to_node(self.nodes[self.unused_next.val].ports[0])
    clear_node(&self.nodes[res.val])
    self.nodes[res.val].kind = kind
    C.printf("end of new_node\n")
    return res
  end
  if self.node_count == self.node_cap then
    self.node_cap = self.node_cap << 1
    self.nodes = realloc(self.nodes, self.node_cap)
  end
  var res = [node_ref]{self.node_count}
  self.node_count = self.node_count + 1
  clear_node(&self.nodes[res.val])
  self.nodes[res.val].kind = kind
  C.printf("end of new_node\n")
  return res
end

M.net.methods.port = macro(function(self, port) return `self.nodes[M.port_to_node(port).val].ports[M.port_to_slot(port)] end)

terra M.net:reuse_node(n: node_ref)
  self:port(M.node_port(n, 0)) = M.node_port(self.unused_next, 1)
  self.unused_next = n
end

terra M.net:enter(port: port_ref)
  return self.nodes[M.port_to_node(port).val].ports[M.port_to_slot(port)]
end

terra M.net:kind(node: node_ref)
  return self.nodes[node.val].kind
end

terra M.net:link(a: port_ref, b: port_ref)
  C.printf("linking %d to %d\n", a.val, b.val)
  self:port(a) = b
  self:port(b) = a
end

terra M.net:new_erase_node()
  var x = self:new_node(0)
  self:link(M.node_port(x, 1), M.node_port(x, 2))
  return M.node_port(x, 0)
end

terra M.default_elim(net: &M.net, x: node_ref, y: node_ref)
  C.printf "applying elim rule"
  escape
    for i = 1, 2 do
      emit quote
          net:link(
            net:enter(M.node_port(x, [i])),
            net:enter(M.node_port(x, [i]))
          )
           end
      end
  end
  net:reuse_node(x)
  net:reuse_node(y)
end

terra M.default_cross(net: &M.net, x: node_ref, y: node_ref)
  C.printf "applying cross rule"
  var a = net:new_node(net:kind(x))
  var b = net:new_node(net:kind(y))

  net:link(M.node_port(b, 0), net:enter(M.node_port(x, 1)))
  net:link(M.node_port(y, 0), net:enter(M.node_port(x, 2)))

  net:link(M.node_port(a, 0), net:enter(M.node_port(y, 1)))
  net:link(M.node_port(x, 0), net:enter(M.node_port(y, 2)))

  net:link(M.node_port(a, 1), M.node_port(b, 1))
  net:link(M.node_port(a, 2), M.node_port(y, 1))
  net:link(M.node_port(x, 1), M.node_port(b, 2))
  net:link(M.node_port(x, 2), M.node_port(y, 2))
end

terra M.defer_other(net: &M.net, x: node_ref, y: node_ref)
  C.printf "defering to other rule"
  net.typeinfo[net:kind(y)].methods[net.typeinfo[net:kind(x)].role](net, y, x)
end

local exit = terralib.externfunction("exit", int -> {})

terra M.crash(net: &M.net, x: node_ref, y: node_ref)
  C.printf("elimination rule for types %d, %d is crash", net:kind(x), net:kind(y))
  exit(1)
end

terra M.new_net()
  var net: M.net
  net.nodes = [&node](malloc([terralib.sizeof(node)]))
  net.nodes[0].ports[0] = [port_ref]{0}
  net.nodes[0].ports[1] = [port_ref]{2}
  net.nodes[0].ports[2] = [port_ref]{1}
  net.nodes[0].kind = 1
  net.node_count = 1
  net.node_cap = 1
  net.unused_next = empty_val
  net.typeinfo = [(`net.typeinfo):gettype()](malloc([terralib.sizeof((`net.typeinfo):gettype().type) *2]))
  net.type_count = 2
  net.type_cap = 2
  escape
    local e, c, o, x = M.default_elim, M.default_cross, M.defer_other, M.crash
    local data = {{r=0, m={e, c, c, x, x}}, {r=1, m={c, e, c, x, x}}}
    for i = 1, 2 do
      for j = 1, 5 do
        emit quote net.typeinfo[ [i-1] ].methods[ [j - 1] ] = [data[i].m[j]] end
      end
      emit quote net.typeinfo[ [i-1] ].role = [data[i].r] end
    end
end
return net
end

terra M.net:new_type()
  if self.type_count == self.type_cap then
    self.type_cap = self.type_cap * 2
    self.typeinfo = realloc(self.typeinfo, self.type_cap)
  end
  var res = self.type_count
  C.printf "creating new type"
  self.type_count = self.type_count + 1
  return res
end

terra M.net:new_dup_type()
  var type_id = self:new_type()
  escape
    local e, c, o = M.default_elim, M.default_cross, M.defer_other
    for i, v in ipairs{c, c, e, o, o} do
      emit quote self.typeinfo[type_id].methods[ [i- 1] ] = v end
    end
  end
  self.typeinfo[type_id].role = 2
  return type_id
end

terra M.net:destroy()
  free(self.nodes)
  free(self.typeinfo)
end

terra M.net:rewrite(x: node_ref, y: node_ref)
  if self:kind(x) > self:kind(y) then
    x, y = y, x
  end
  self.typeinfo[self:kind(x)].methods[self.typeinfo[self:kind(y)].role](self, x, y)
end

terra M.net:reduce()
  var loops = 0
  var rules = 0
  var warp: Stack(port_ref)
  warp:init()
  defer warp:destroy()
  var exit: Stack(slot_ref)
  exit:init()
  defer exit:destroy()
  var prev: port_ref
  var back: port_ref
  var next: port_ref
  next = self.nodes[0].ports[0]
  while next.val > 0 or not warp:empty() do
    if next.val == 0 then
      next = self:enter(warp:pop())
    end
    prev = self:enter(next)
    C.printf("prev %d, next %d\n", prev.val, next.val)
    if M.port_to_slot(next) == 0 then
      if M.port_to_slot(prev) == 0 and M.port_to_node(prev).val ~= 0 then
        rules = rules + 1
        back = self:enter(M.node_port(M.port_to_node(prev), exit:pop()))
        self:rewrite(M.port_to_node(prev), M.port_to_node(next))
        next = self:enter(back)
      else
        warp:push(M.node_port(M.port_to_node(next), 2))
        next = M.node_port(M.port_to_node(next), 1)
      end
    else
      exit:push(M.port_to_slot(next))
      next = self:enter(M.node_port(M.port_to_node(next), 0))
    end
    loops = loops + 1
  end
  return {loops, rules}
end

return M
