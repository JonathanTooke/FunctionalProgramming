delete :: Int -> [Int] -> [Int]
delete x [] = []
delete x (y:ys) | x == y = ys
                | otherwise = y : delete x ys