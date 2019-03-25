
data Expr = Uni        -- | Unit        | ()      |   
  | Var String         -- | Variable    | x       |  
  | Lam String Expr    -- | Lambda      | \x -> e |  
  | App Expr Expr      -- | Application | e e     | 
  | Ano Expr PolyType  -- | Annotation  | (e : a) |  
  deriving (Show, Eq)
  
data PolyType = UnitType           -- | Unit Type      | 1             | 
  | VarType String                 -- | Type Variables | alpha          | 
  | ForAllType String PolyType     -- | ForAll alpha a | 
  | PolyFuncType PolyType PolyType -- | a -> b         | 
  deriving (Show, Eq)

type Context = [ContextElement]
data ContextElement = UnitElem 
  | TypeElem String 
  | ProgVarElem String PolyType    --REQUIRED FOR LAMDA
  deriving (Show, Eq)
--  | ExVar TypeVar         
--  | SolvedVar TypeVar 
--  | ScopeMarkers? (Looks like an arrow and and A-Hat), represents type variables


-------todo-----
-- WELL FORMED Context Fig.7
-- Write a function called subtype, takes 2 types, returns Bool. Returns true when the types are equal

lookupProgVar :: Context -> String -> Maybe PolyType
lookupProgVar [] _ = Nothing
lookupProgVar ((ProgVarElem name polytype):others) search_name
  | name == search_name = Just polytype
  | otherwise = lookupProgVar others search_name
lookupProgVar (_:others) search_name = lookupProgVar others search_name

isSubType :: Context -> PolyType -> PolyType -> Bool
isSubType _ UnitType UnitType = True -- SUBTYPE Unit
isSubType ctx (VarType a) (VarType b) =  isEqualType ctx (VarType a) (VarType b) -- SUBTYPE Var
isSubType ctx (PolyFuncType a1 a2) (PolyFuncType b1 b2) = (isSubType ctx b1 a1) && (isSubType ctx a2 b2) -- SUBTYPE PolyFuncType
isSubType ctx (ForAllType alpha a) b  = undefined
isSubType ctx  a (ForAllType beta  b) = undefined
isSubType _ _ _ = undefined

isEqualType :: Context -> PolyType -> PolyType -> Bool
isEqualType _ UnitType UnitType = True
isEqualType ctx (VarType a) (VarType b) = (a == b) && (contextContainsTypeElem ctx a) && (contextContainsTypeElem ctx b)


contextContainsTypeElem :: Context -> String -> Bool
contextContainsTypeElem [] _ = False
contextContainsTypeElem ((TypeElem a) :others) b
  | a == b = True
  | otherwise = contextContainsTypeElem others b

isWellFormed :: Context -> PolyType -> Bool
isWellFormed _ UnitType = True -- DeclUnitWF  
isWellFormed ctx (VarType a)        = contextContainsTypeElem ctx a                -- DeclUVarWF
isWellFormed ctx (PolyFuncType a b) = (isWellFormed ctx a) && (isWellFormed ctx b) -- DeclArrowWF 
isWellFormed ctx (ForAllType a b)   = isWellFormed ((TypeElem a):ctx) b            -- DeclForallWF 

check :: Context -> Expr -> PolyType -> Bool
check _ Uni _ = True  -- rule Decl1L_check
check ctx (Lam x e) (PolyFuncType a b) = check ((ProgVarElem x a) : ctx) e b -- rule Decl->I
-- For DeclSub, how do you search for and determine the values for A and B?
check ctx e (ForAllType alpha a) = check ((TypeElem alpha) : ctx) e a -- DeclForAllIntro


synth :: Context -> Expr -> Maybe PolyType 
synth _ Uni = Just UnitType               -- rule Decl1_Intro_synth 
synth ctx (Var x)   = lookupProgVar ctx x -- rule DeclAnno         
synth _ (Lam x  e)  = undefined           -- rule Decl->Intro_synth
  -- rule Monotype o -> t if o -> t is well formed, and e checks against theta, given context + (x : omega) annotation
  -- How are monotypes omega and theta determined? Is this the magic code?

synth ctx (App e1 e2) = apply_synth ctx (synth ctx e1) e2 -- rule Decl->E
synth ctx (Ano e a)
  | isWellFormed ctx a && check ctx e a = Just a
  | otherwise = Nothing

apply_synth :: Context -> Maybe PolyType -> Expr -> Maybe PolyType
apply_synth _ _ _ = undefined

-- FOR THE NEXT STEP
-- For a rewrite: 
--   make checking return Maybe ( Context ) 
--   make synth    return Maybe ( Type, Context ) 



-- 1 Implement UnitTypes and FunctionTypes
-- 2 Implement Alpha and Forall
-- 3 Implement Alpha-Hat


-- EXAMPLE TEST CODES:
-- Uni -> Just UnitType
-- Var "a"

-- EXAMPLES!
-- . |- (lambda x.x : forall alpha -> alpha) => ?
-- . |- ( ():1 ) => ?
-- . |- ( (lambda x. x) : (1 ->1) ) => ?
-- . |- () () => ?
-- . |- ( (lambda x. x) ) => ?

-- Eventually write a function to check whether a context is well formed (Don't have 

