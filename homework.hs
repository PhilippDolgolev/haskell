
prim' n d | n `div` 2 < d = True
prim' n d | n `mod` d == 0 = False
prim' n d = prim' n (succ d)
prim' n = prim' n 2


rprim a b | gcd a b == 1 = True
rprim a b = False

lcm a b | a == 0 || b == 0 = 0
lcm a b = a * b `div` (gcd a b)

nd' n d acc | n < d = acc
nd' n d acc | n `mod` d == 0 = nd' n (succ d) (succ acc)
nd' n d acc = nd' n (succ d) acc
nd n = nd' n 2 1


sd' n d acc | n == 0 = 0
sd' n d acc | n < d = acc
sd' n d acc | n `mod` d == 0 = sd' n (succ d) (acc + d)
sd' n d acc = sd' n (succ d) acc
sd n = sd' n 2 1

howManyCanDivide' n d acc | n `mod` d == 0 = howManyCanDivide' (n `div` d) d (succ acc)
howManyCanDivide' n d acc = acc
howManyCanDivide n d = howManyCanDivide' n d 0

euler' n d acc | n < d = acc
euler' n d acc | c > 0 = euler' (n `div` st) (succ d) (acc * (st - d^(c-1))) 
  where c = howManyCanDivide n d
        st = d ^ c
euler' n d acc = euler' n (succ d) acc
euler n = euler' n 2 1


