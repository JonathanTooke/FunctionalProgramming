split :: [Int] -> [([Int],[Int])]
split (xs) = [(take a xs, drop a xs)| a <- [1..b]]
            where
                b = length xs - 1