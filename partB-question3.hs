delete :: Int -> [Int] -> [Int]
delete x [] = []
delete x (y:ys) | x == y = ys
                | otherwise = y : delete x ys

perms :: [Int] -> [[Int]]
perms [] = [[]]
perms xs = [a:bs| a <- xs, bs <- perms (delete a xs)] 