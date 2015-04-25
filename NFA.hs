import Data.List

data Q = Q0 | Q1 | Q2 deriving (Eq)
data S = O | I

delta :: Q -> S -> [Q]
delta Q0 O = [Q0]
delta Q0 I = [Q0,Q1]
delta Q1 _ = [Q2]
delta Q2 _ = []

run :: Q -> [S] -> [Q]
run q []         = [q]
run q (s:ss)     = concat $ [run x ss | x <- xs]
    where xs = delta q s

test :: Q -> [S] -> Bool
test q s = elem Q2 (run q s)
