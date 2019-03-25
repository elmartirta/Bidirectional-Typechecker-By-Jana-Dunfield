data Expr = Lit Float 
  | Add Expr Expr 
  | Sub Expr Expr 
  | Mul Expr Expr 
  | Div Expr Expr
  | Pwr Expr Expr
  deriving (Eq,Show)

data BExpr = BoolLit Bool
  | And BExpr BExpr
  | Not BExpr
  | Eql Expr Expr
  | Gtr Expr Expr
  | Les Expr Expr

-- (1 + (2 + (3 + 4, right assosiative
-- eq1 = Add(Lit 1 Add (Lit 2 Add (Lit 3 Lit 4)))

-- eq1 = Lit 1
-- eq2 = Add (Lit 1) (Lit 2)
-- -- eq3 = Add (Lit 1) (Add (Lit 2) (Add (Lit 3) (Lit 4)))
-- eq4 = Gtr (Add (Lit 1) (Lit 2)) (Add (Lit 3) (Lit 4))


eval :: Expr -> Float
eval (Lit a) = a
eval (Add a b) = (eval a) + (eval b)
eval (Sub a b) = (eval a) - (eval b)
eval (Mul a b) = (eval a) * (eval b)
eval (Div a b) = (eval a) / (eval b)
eval (Pwr a b) = (eval a) ** (eval b)

bEval :: BExpr -> Bool
bEval (BoolLit a) = a
bEval (And a b) = bEval(a) && bEval(b)
bEval (Not a) = not (bEval(a))
bEval (Eql a b) = eval(a) == eval(b)
bEval (Gtr a b) = eval(a) >  eval(b)
bEval (Les a b) = eval(a) <  eval(b)
