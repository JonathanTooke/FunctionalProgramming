data Expr = Val Int | App Op Expr Expr
data Op = Add | Mul

eval :: Expr -> Int
eval (Val x) = x
eval (App Add a b) = eval a + eval b
eval (App Mul a b) = eval a * eval b

values :: Expr -> [ Int ]
values (Val x) = [x]
values (App Add l r) = values(l) ++ values(r)
values (App Mul l r) = values(l) ++ values(r)
