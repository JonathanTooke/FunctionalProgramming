prod :: Num a => [a] -> a
prod [] = 1
prod (n:ns) = n * product(ns)