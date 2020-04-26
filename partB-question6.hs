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

eval :: Expr -> Int
eval (Val x) = x
eval (App Add a b) = eval a + eval b
eval (App Mul a b) = eval a * eval b

values :: Expr -> [ Int ]
values (Val x) = [x]
values (App Add l r) = values(l) ++ values(r)
values (App Mul l r) = values(l) ++ values(r)

delete :: Int -> [Int] -> [Int]
delete x [] = []
delete x (y:ys) | x == y = ys
                | otherwise = y : delete x ys

perms :: [Int] -> [[Int]]
perms [] = [[]]
perms xs = [a:bs| a <- xs, bs <- perms (delete a xs)] 

solve :: [Int] -> Int -> [Expr]
solve [] y = []
solve xs y = [x| ps <- perms xs, x <- exprs ps, eval x == y]