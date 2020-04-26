data Expr = Val Int | App Op Expr Expr
    deriving Show

data Op = Add | Mul
    deriving Show

split :: [Int] -> [([Int],[Int])]
split (xs) = [(take a xs, drop a xs)| a <- [1..b]]
            where
                b = length xs - 1

exprs :: [ Int ] -> [ Expr ]
exprs [] = []
exprs [x] = [Val x]
exprs xs = [expr | (ls,rs) <- split xs, le <- exprs ls, re <- exprs rs, expr <- [App o le re | o <- [Add,Mul]]]