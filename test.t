local lp = require 'lambda-parser'


local lam = lp.parse_lambda("(\\ a -> a ) (\\ a -> a)")

print(lam)

local net = lp.lambda_to_net(lam)

print("net", net)
lp.dump_net(net)

local recovered = lp.net_to_string(net)

print(recovered)

local stats = net:reduce()
print("reduce", stats._0, stats._1)

print(lp.net_to_string(net))
