--IMPLEMENTATION OF THE ALGORITHMIC SYSTEM

-- TODO: Implement a different type, replacing String
-- TODO: Try to find bugs, that confuse alpha and alpha-hat
--       Or, implement a different data type, for alpha and alpha-hat

newtype ExVarName = ExVarName String deriving(Show, Eq)
newtype UniVarName = UniVarName String deriving(Show, Eq)

data Expr = 
    Uni                -- | Unit        | ()      |   
  | Var String         -- | Variable    | x       |  
  | Lam String Expr    -- | Lambda      | \x -> e |  
  | App Expr Expr      -- | Application | e e     | 
  | Ano Expr PolyType  -- | Annotation  | (e : a) |  
  deriving (Show, Eq)

data PolyType = 
    UnitType                    -- | Unit Type            | 1              | 
  | UnivType UniVarName         -- | Universal TypeVars   | Alpha          | 
  | ExType ExVarName            -- | Existential TypeVars | Alpha-Hat      |
  | ForAll UniVarName PolyType  -- | ForAll Type          | V.Alpha A      |  -- ??? Can ForAll use V.Alpha-hat?
  | Func PolyType PolyType      -- | Arrow                | A -> B         |
  deriving (Show, Eq)
  
type Context = [ContextElement]
data ContextElement =
    UnivTypeElem UniVarName         -- | Universal Type Variables   | Alpha
  | ProgVar String PolyType         -- | Term Variable Typings      | x:A
  | ExVar ExVarName                 -- | Existential Type Variables | Alpha-Hat
  | SolvedExVar ExVarName PolyType  -- | Solved X Type Variables    | Alpha-Hat = Tau
  | Marker ExVarName                -- | Marker                     | >alpha-hat
  deriving (Show, Eq)

-- Ruled by Substitution
applyContextToType :: Context -> PolyType -> PolyType
applyContextToType _ UnitType = UnitType 
applyContextToType _ (UnivType a) = UnivType a
applyContextToType ctx (ExType a_hat) = case lookupExVar ctx a_hat of
  Just(SolvedExVar _ tau) -> applyContextToType ctx tau
  Just(ExVar a_hat) -> (ExType a_hat)
  Nothing -> error ("Context: " ++ show ctx ++ "Applied to missing ExType: " ++ show (ExType a_hat))
  _       -> error ("Context: " ++ show ctx ++ "Applied to missing ExType: " ++ show (ExType a_hat))
applyContextToType ctx (Func a b) = Func (applyContextToType ctx a) (applyContextToType ctx b)
applyContextToType ctx (ForAll alpha a) = ForAll alpha (applyContextToType ctx a)



-- everything is reversed, the head of the list is the rightmost part of the list
-- Takes a context, and an alpha, and splits it into ctx_old alpha ctx_new, returning (ctx_old, ctx_new)
-- splitContextOn :: Context -> ContextElement -> Maybe (Context, Context)
-- splitContextOn [] _ = Nothing
-- splitContextOn item:ctx elem = 
--   |
-- novelAlphaHat :: Context -> Integer -> Context 
-- check if alpha_hat_x is in context, if it is, increment x, and try again

novelAlphaHat :: Context -> ExVarName
novelAlphaHat ctx = go ctx ctx 0
  where  -- Todo: Simplify the ugliest code I've written in my entire life. Hitchhike on existInDom perhaps? Reduce "A^" ++ show hat_num repetition.
  go full_ctx [] hat_num = ExVarName("A^" ++ show hat_num)
  go full_ctx ((SolvedExVar (ExVarName beta_hat) _):working_ctx) hat_num
    | (beta_hat == ("A^" ++ show(hat_num))) = go full_ctx full_ctx (hat_num + 1)
    | otherwise = go full_ctx working_ctx hat_num
  go full_ctx ((ExVar (ExVarName beta_hat)):working_ctx) hat_num
    | (beta_hat == ("A^" ++ show(hat_num))) = go full_ctx full_ctx (hat_num + 1)
    | otherwise = go full_ctx working_ctx hat_num
  go full_ctx ((Marker(ExVarName beta_hat)):working_ctx) hat_num
    | (beta_hat == ("A^" ++ show(hat_num))) = go full_ctx full_ctx (hat_num + 1)
    | otherwise = go full_ctx working_ctx hat_num
  go full_ctx (_:working_ctx) hat_num = go full_ctx working_ctx hat_num

-- ??? How do you treat missing elements? Should probably just crash
splitContextOn :: Context -> ContextElement -> (Context, Context)
splitContextOn ctx elem = go elem ctx []
  where 
    go elem [] new_reversed = error ("Element:" ++ show elem ++"is is missing from context to be split:" ++ show ctx)
    go elem (item:old) new_reversed
      | (existInDom [elem] item) = (reverse new_reversed, old)
      | otherwise = go elem old (item:new_reversed)


-- ??? Is this how you represent [a_hat/a]A, in ForAll Application in Figure 11?
replaceVarWithExVarInType :: UniVarName -> ExVarName -> PolyType -> PolyType
replaceVarWithExVarInType alpha alpha_hat a = case a of
  UnitType -> UnitType
  UnivType beta -> case alpha == beta of
    True -> ExType alpha_hat
    False -> UnivType beta
  ExType beta_hat -> ExType beta_hat
  ForAll a b -> undefined -- ForAll a (replaceVarWithExVarInType alpha alpha_hat b) 
  -- ^ ??? ForAll only accepts alpha as its first parameter, how do you replace 
  -- alpha with alpha_hat in here? Is it legal to have a ForAll alpha_hat?
  Func a b -> Func (replaceVarWithExVarInType alpha alpha_hat a) (replaceVarWithExVarInType alpha alpha_hat b)

-- Ruled by Figure 7
isTypeWellFormed :: Context -> PolyType -> Bool
isTypeWellFormed _   (UnitType) = True 
isTypeWellFormed ctx (UnivType a) = (existInDom ctx (UnivTypeElem a)) 
isTypeWellFormed ctx (ExType alphahat) = (existInDom ctx (ExVar alphahat))
isTypeWellFormed ctx (Func a b) = (isTypeWellFormed ctx a) && (isTypeWellFormed ctx b)
isTypeWellFormed ctx (ForAll alpha a) = isTypeWellFormed ((UnivTypeElem alpha):ctx) a

-- Ruled by Figure 7
isContextWellFormed :: Context -> Bool
isContextWellFormed [] = True 
isContextWellFormed ((UnivTypeElem alpha):ctx) = (not(existInDom ctx (UnivTypeElem alpha))) && (isContextWellFormed ctx) -- UVar Ctx
isContextWellFormed ((ExVar a_hat):ctx) = (not(existInDom ctx (ExVar a_hat))) && (isTypeWellFormed ctx (ExType a_hat)) && (isContextWellFormed ctx) -- ExType Ctx
isContextWellFormed ((SolvedExVar a t):ctx) = (not(existInDom ctx (SolvedExVar a t))) && (isTypeWellFormed ctx t) && (isContextWellFormed ctx) -- Solved Evar ctx
isContextWellFormed ((ProgVar x a):ctx) = (not(existInDom ctx (ProgVar x a))) && (isContextWellFormed ctx) -- Var Ctx
isContextWellFormed ((Marker a_hat):ctx) = (not(existInDom ctx (ExVar a_hat))) && (not(existInDom ctx (Marker a_hat))) && (isContextWellFormed ctx) -- Marker Ctx



-- TODO: Lookup for every kind of thing: ProgVar, and ExTypes

lookupProgVar :: Context -> String -> Maybe ContextElement
lookupProgVar [] name = Nothing
lookupProgVar ((ProgVar x a):others) name
  | x == name = Just (ProgVar x a)
  | otherwise = lookupProgVar others name
lookupProgVar (_:others) name = lookupProgVar others name

lookupExVar :: Context -> ExVarName -> Maybe ContextElement
lookupExVar [] _ = Nothing
lookupExVar (item:ctx) alpha_hat = case item of
  ExVar beta_hat -> case (alpha_hat == beta_hat) of
    True -> Just (ExVar beta_hat)
    False -> continue
  SolvedExVar beta_hat tau -> case (alpha_hat == beta_hat) of
    True -> Just(SolvedExVar beta_hat tau)
    False -> continue
  _  -> continue
  where continue = lookupExVar ctx alpha_hat

-- Exist In Domain Check
existInDom :: Context -> ContextElement -> Bool 
existInDom [] _ = False

-- ExistInDomain Check: UnitType
existInDom (UnivTypeElem beta:ctx) (UnivTypeElem alpha)
  | (beta == alpha) = True
  | otherwise = existInDom ctx (UnivTypeElem alpha)
existInDom (_:ctx) (UnivTypeElem alpha) = existInDom ctx (UnivTypeElem alpha)

-- ExistInDomain Check: ProgVar
existInDom ctx (ProgVar x a) = 
  case lookupProgVar ctx x of 
     Nothing -> False
     Just _  -> True 

-- ExistInDomain Check: ExVar
existInDom (ExVar beta_hat:ctx) (ExVar alpha_hat)
  | (beta_hat == alpha_hat) = True
  | otherwise = existInDom ctx (ExVar alpha_hat)
existInDom (SolvedExVar beta_hat _:ctx) (ExVar alpha_hat)
  | (beta_hat == alpha_hat) = True
  | otherwise = existInDom ctx (ExVar alpha_hat)
existInDom (_:ctx) (ExVar alpha_hat) = existInDom ctx (ExVar alpha_hat)

-- ExistInDomain Check: SolvedExVar
existInDom (ExVar beta_hat:ctx) (SolvedExVar alpha_hat tau)
  | (beta_hat == alpha_hat) = True
  | otherwise = existInDom ctx (SolvedExVar alpha_hat tau)
existInDom (SolvedExVar beta_hat _:ctx) (SolvedExVar alpha_hat tau)
  | (beta_hat == alpha_hat) = True
  | otherwise = existInDom ctx (SolvedExVar alpha_hat tau)
existInDom (_:ctx) (SolvedExVar alpha_hat tau) = existInDom ctx (SolvedExVar alpha_hat tau)

-- ExistInDomain Check: Marker
existInDom (Marker beta_hat:ctx) (Marker alpha_hat)
  | (beta_hat == alpha_hat) = True
  | otherwise = existInDom ctx (Marker alpha_hat)
existInDom (_:ctx) (Marker alpha_hat) = existInDom ctx (Marker alpha_hat)

inFreeVariables :: ExVarName -> PolyType -> Bool
inFreeVariables alpha_hat UnitType = False
inFreeVariables alpha_hat (UnivType a) = undefined
inFreeVariables alpha_hat (ExType a) = undefined
inFreeVariables alpha_hat (ForAll a) = undefined
inFreeVariables alpha_hat (Func a b) = undefined

isMonotype :: PolyType -> Bool
isMonotype _ = undefined -- ??? 

-- Ruled by Figure 11
check :: Context -> Expr -> PolyType -> Maybe (Bool, Context)

-- Fig 11 - Unit Introduction
check ctx Uni (UnitType) = Just(True, ctx)

-- Fig 11 - SubTyping
check ctx e (UnivType b) = 
  case synth ctx e of 
    Nothing -> Nothing
    Just(a, theta) -> case subType theta (applyContextToType theta a) (applyContextToType theta (UnivType b)) of
      Nothing -> Nothing
      Just(delta) -> Just(True, delta)

-- Fig 11 - SubTyping      
check ctx e (ExType b) = 
  case synth ctx e of 
    Nothing -> Nothing
    Just(a, theta) -> case subType theta (applyContextToType theta a) (applyContextToType theta (ExType b)) of
      Nothing -> Nothing
      Just(delta) -> Just(True, delta)
      
-- Ruled by Figure 11 - ForAll Introduction
check ctx e (ForAll alpha a) = 
  case check ((UnivTypeElem alpha):ctx) e a of 
    Nothing -> Nothing
    Just (_,_) -> undefined

-- Ruled by Figure 11 - Arrow Introduction
check ctx (Lam x e) (Func a b) =
  case check ((ProgVar x a):ctx) e b of 
  Nothing -> Nothing
  Just (False, _) -> Nothing
  Just (True, out_ctx) -> case splitContextOn out_ctx (ProgVar x a) of 
    (theta, delta) -> Just (True, delta)  
  
check _ _ _ = undefined

-- Ruled by Figure 11 - Unit Introduction
synth :: Context -> Expr ->  Maybe(PolyType, Context)
synth ctx (Uni) = Just (UnitType, ctx)

-- Ruled by Figure 11 - Var Introduction
synth ctx (Var x) = 
  case lookupProgVar ctx x of
    Nothing -> Nothing
    Just (ProgVar _ a) -> Just (a, ctx)
      
-- Ruled by Figure 11 - Annotation Introduction
synth ctx (Ano e a) = case isTypeWellFormed ctx a of
    False -> Nothing
    True -> case check ctx e a of
      Nothing -> Nothing
      Just(False, _) -> Nothing
      Just(True, delta) -> Just (a, delta)

-- Ruled by Fig 11 - Arrow Introduction
synth ctx (Lam x e) = 
  let alpha_hat = novelAlphaHat(ctx) in 
  let beta_hat  = novelAlphaHat((ExVar alpha_hat):ctx) in 
  case check ((ProgVar x (ExType alpha_hat)):(ExVar beta_hat):(ExVar alpha_hat):ctx) e (ExType beta_hat) of
    Nothing -> Nothing
    Just (False, _) -> Nothing
    Just (True, out_ctx) -> case splitContextOn out_ctx (ProgVar x (ExType alpha_hat)) of
      (theta, delta) -> Just (Func (ExType alpha_hat) (ExType beta_hat), delta)
      
-- Ruled by Fig 11 - Arrow Elimination
synth ctx (App e1 e2) = case synth ctx e1 of 
    Nothing -> Nothing 
    Just(a, theta) -> case apply theta (applyContextToType theta a) e2 of
      Nothing -> Nothing
      Just (c, delta) -> Just (c, delta)
    
-- Ruled by Figure 11
apply :: Context -> PolyType -> Expr -> Maybe (PolyType, Context)

-- ForAll Application
apply ctx (ForAll alpha a) e =
  let alpha_hat = novelAlphaHat ctx in 
  case apply (ExVar alpha_hat:ctx) (replaceVarWithExVarInType alpha alpha_hat a) e of
    Nothing -> Nothing 
    Just (c, delta) -> Just (c, delta)

-- Alpha Application
apply ctx (ExType a_hat) e =
  let a_hat_1 = novelAlphaHat ctx in
  let a_hat_2 = novelAlphaHat ((ExVar a_hat_1):ctx) in 
  case splitContextOn ctx (ExVar a_hat) of
    (new, old) -> case check (new++(SolvedExVar a_hat (Func (ExType a_hat_1) (ExType a_hat_2)):(ExVar a_hat_1):(ExVar a_hat_2):old)) e (ExType a_hat_1) of 
      Nothing -> Nothing
      Just (False, _) -> Nothing
      Just (True, delta) -> Just(ExType a_hat_2, delta)
  
-- Ruled by Figure 11 - Arrow Application
apply ctx (Func a c) e = case check ctx e a of
  Nothing -> Nothing
  Just(False, _) -> Nothing
  Just(True, delta) -> Just(c, delta)
  
apply _ _ _ = undefined
-- TODO: Replace these with case

subType :: Context -> PolyType -> PolyType -> Maybe (Context)

-- Figure 9 - Unit
subType ctx UnitType UnitType = Just ctx

-- Figure 9 - Var
subType ctx (UnivType a) (UnivType b)
  | (existInDom ctx (UnivTypeElem a)) && (a == b) = Just ctx
  | otherwise = Nothing

-- Figure 9 - ExVar
subType ctx (ExType alpha_hat) (ExType beta_hat)
  | (existInDom ctx (ExVar alpha_hat)) && (alpha_hat == beta_hat) = Just ctx
  | otherwise = Nothing 
  
-- Figure 9 - Func
subType ctx (Func a1 a2) (Func b1 b2) = case subType ctx b1 a1 of
  Nothing -> Nothing
  Just theta -> case subType theta (applyContextToType theta a2) (applyContextToType theta b2) of
    Nothing -> Nothing
    Just delta -> Just delta

-- Figure 9 - ForAll Left
subType ctx (ForAll alpha a) b = 
  let alpha_hat = novelAlphaHat(ctx) in 
  case subType ((ExVar alpha_hat):(Marker alpha_hat):ctx) (replaceVarWithExVarInType alpha alpha_hat a) b of
    Nothing -> Nothing
    Just out_ctx -> case splitContextOn out_ctx (Marker alpha_hat)  of
      (theta, delta) -> Just delta 

-- Figure 9 - ForAll Right
subType ctx a (ForAll alpha b) = case subType ((UnivTypeElem alpha):ctx) a b of
  Nothing -> Nothing
  Just out_ctx -> case splitContextOn out_ctx (UnivTypeElem alpha) of
    (theta, delta) -> Just delta

-- Figure 9 - Instantiate Left
subType ctx (ExType alpha_hat) a = case inFreeVariables alpha_hat a of 
  True -> Nothing
  False -> case instantiateLeft ctx alpha_hat a of
    Nothing -> Nothing
    Just delta -> Just delta

-- Figure 9 - Instantiate Right 
subType ctx a (ExType alpha_hat) = case inFreeVariables alpha_hat a of 
  True -> Nothing
  False -> case instantiateRight ctx a alpha_hat of
    Nothing -> Nothing
    Just delta -> Just delta
    
subType _ _ _ = undefined

instantiateLeft :: Context -> ExVarName -> PolyType -> Maybe Context

-- Figure 10 - Instantiate Left Solve 
instantiateLeft ctx alpha_hat (UnivType tau) = case splitContextOn ctx (ExVar alpha_hat) of
  (ctx_new, ctx_old) -> case ((isTypeWellFormed ctx (UnivType tau)) && (isMonotype (UnivType tau))) of  
    -- ??? Mono-type troubles: How do you find out what is a Mono-type at this point?
    False -> Nothing
    True -> Just (ctx_new++(SolvedExVar alpha_hat (UnivType tau)):ctx_old)
    
-- Figure 10 - Instantiate Left Reach
instantiateLeft ctx alpha_hat (ExType beta_hat) = case splitContextOn ctx (ExVar beta_hat) of 
  -- ??? Potential for error: Will Beta_hat and alpha_hat always exist in context?
  (ctx_new, ctx_old) -> case splitContextOn ctx (ExVar alpha_hat) of
    (_, ctx_older) -> Just(ctx_new ++ (SolvedExVar beta_hat (ExType alpha_hat)):ctx_old ++ (ExVar alpha_hat):ctx_older)

-- Figure 10 - Instantiate Left Arr
instantiateLeft ctx a_hat (Func a1 a2) = 
  let a_hat_1 = novelAlphaHat (ctx) in
  let a_hat_2 = novelAlphaHat ((ExVar a_hat_1):ctx) in 
  case splitContextOn ctx (ExVar a_hat) of
    (new, old) -> let changed_ctx = (new++(SolvedExVar a_hat (Func (ExType a_hat_1) (ExType a_hat_2)):(ExVar a_hat_1):(ExVar a_hat_2):old)) in 
      case instantiateRight changed_ctx a1 a_hat_1 of 
      Nothing -> Nothing
      Just theta -> case instantiateRight theta (applyContextToType theta a2) a_hat_2 of
        Nothing -> Nothing
        Just delta -> Just delta
      
-- Figure 10 - Instantiate Left A||R
instantiateLeft ctx alpha_hat (ForAll beta b) = case lookupExVar ctx alpha_hat of  
  Nothing -> Nothing
  Just(SolvedExVar _ _) -> Nothing
  Just(ExVar _) -> case instantiateLeft ((UnivTypeElem beta):ctx) alpha_hat b of
    Nothing -> Nothing
    Just out_ctx -> case splitContextOn out_ctx (UnivTypeElem beta) of
      (delta_prime, delta) -> Just delta



instantiateLeft _ _ _ = undefined

instantiateRight :: Context -> PolyType -> ExVarName -> Maybe Context

-- Figure 10 - Instantiate Right Solve 
instantiateRight ctx (UnivType tau) alpha_hat = instantiateLeft ctx alpha_hat (UnivType tau)
    
-- Figure 10 - Instantiate Right Reach
instantiateRight ctx (ExType beta_hat) alpha_hat = instantiateLeft ctx alpha_hat (ExType beta_hat)

-- Figure 10 - 
instantiateRight ctx (Func a1 a2) a_hat = 
  let a_hat_1 = novelAlphaHat (ctx) in
  let a_hat_2 = novelAlphaHat ((ExVar a_hat_1):ctx) in 
  case splitContextOn ctx (ExVar a_hat) of
    (new, old) -> let changed_ctx = (new++(SolvedExVar a_hat (Func (ExType a_hat_1) (ExType a_hat_2)):(ExVar a_hat_1):(ExVar a_hat_2):old)) in 
      case instantiateLeft changed_ctx a_hat_1 a1 of 
      Nothing -> Nothing
      Just theta -> case instantiateRight theta (applyContextToType theta a2) a_hat_2 of
        Nothing -> Nothing
        Just delta -> Just delta
        
instantiateRight ctx (ForAll beta b) alpha_hat = case lookupExVar ctx alpha_hat of
  Nothing -> Nothing
  Just (SolvedExVar _ _ ) -> Nothing
  Just (ExVar _) -> let beta_hat = novelAlphaHat(ctx) in 
    case instantiateRight ((ExVar beta_hat):(Marker beta_hat):ctx) (replaceVarWithExVarInType beta beta_hat b) alpha_hat of
      Nothing -> Nothing
      Just (out_ctx) -> case splitContextOn out_ctx (Marker beta_hat) of
        (delta_prime, delta) -> Just (delta)
instantiateRight _ _ _ = undefined

-- BASIC TEST CASES
test1 = isContextWellFormed []
test2 = undefined
test3 = isContextWellFormed [UnivTypeElem (UniVarName "A")]
test4 = not (isContextWellFormed [UnivTypeElem (UniVarName "A"), UnivTypeElem (UniVarName "A")])
test5 = isContextWellFormed [Marker (ExVarName "A^")]
test6 = isContextWellFormed [Marker (ExVarName "A^"), UnivTypeElem (UniVarName "A")]
test7 = undefined -- CREATE TEST WHERE ALPHA-HAT AND ALPHA-HAT IS DECLARED TWICE, solved and unsolved
test8 = undefined -- Create a test where alpha-hat and alpha share same name, is ok
-- GOOD CONTEXTS [ExVar "A^", ProgVar "x" TYPE]
-- ??? CONTEXTS  [ExVar "A^", ProgVar "A" TYPE] only okay if these are treated differently


-- Contexts to test on [Marker (ExVarName "A"), UnivTypeElem "B", UnivTypeElem "A", SolvedExVar (ExVarName "B") UnitType, ProgVar "B" (UnitType)]
printContext = undefined -- TODO 







