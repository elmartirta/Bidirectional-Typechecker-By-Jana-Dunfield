module ProjectTester where

import AlgorithmicTypeChecker

-- TEST Synth context expression

data Test = 
    SynthTest Context Expr (Maybe(Maybe(PolyType, Context)))
  | CheckTest Context Expr PolyType (Maybe(Maybe(Bool, Context)))
  | ApplyTest Context PolyType Expr (Maybe(Maybe(PolyType, Context)))
  | WellFormedContextTest Context (Maybe(Bool))
  | WellFormedTypeTest Context PolyType (Maybe(Bool))
  deriving (Show, Eq)
  

alpha_elem = UnivTypeElem (UniVarName "Alpha")
a_hat_marker = Marker (ExVarName "A^")

tests = [
  WellFormedContextTest [] (Just True),
  WellFormedContextTest [alpha_elem] (Just True),
  WellFormedContextTest [a_hat_marker] (Just True),
  WellFormedContextTest [alpha_elem, alpha_elem] (Just False),
  WellFormedContextTest [a_hat_marker, a_hat_marker] (Just False),
  SynthTest [] Uni (Just(Just(UnitType,[])))
  ]


-- putStr(unlines(map show (run tests)))
run :: [Test] -> [Bool]
run [] = []
run ((WellFormedContextTest ctx desired_output):others)  = case desired_output of
  Nothing -> isContextWellFormed ctx:(run others)
  (Just desired_bool) -> (isContextWellFormed ctx == desired_bool):(run others) 
run _= undefined
-- GOOD CONTEXTS [ExVar "A^", ProgVar "x" TYPE]
-- Mystery CONTEXTS  [ExVar "A^", ProgVar "A" TYPE] only okay if these are treated differently

testExpr :: Int -> Expr
testExpr 0 = Uni
testExpr 1 = Var "A"
testExpr 2 = Lam "X" Uni
testExpr 3 = App Uni Uni
testExpr 4 = Ano Uni UnitType
testExpr 5 = App (Lam "X" Uni) (Uni)
testExpr n = error ("There does not exist any expression of number" ++ show n)

testContext :: Int -> Context
testContext n = error ("There does not exist any context of number" ++ show n)

-- Contexts to test on [Marker (ExVarName "A"), UnivTypeElem "B", UnivTypeElem "A", SolvedExVar (ExVarName "B") UnitType, ProgVar "B" (UnitType)]

-- Add J Dunfield to the github repository

-- The output context should always be well-formed
-- The output type should be well formed under the output context
-- If you are checking, the input type should be well-formed in the input context
-- The input context should be well-formed.

-- Any valid input, should not cause the program to crash.
-- Limitation: Interesting code, usually has variables in it.
-- Create a type-correct program, and then randomly change it, mutation testing.
