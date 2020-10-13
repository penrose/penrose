-- | "Substance" contains the grammar, parser, and semantic checker for
--   the Substance language. It also contains translators to Alloy and
--   the driver for it.
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE FlexibleContexts #-}

module Penrose.Substance where

--module Main (main) where -- for debugging purposes
-- TODO split this up + do selective export
import           Control.Arrow                  ((>>>))
import           Control.Monad                  (void)
import           Control.Monad.Combinators.Expr hiding (Operator)
import           Control.Monad.State.Lazy       (evalStateT, get)
import           Data.List
import qualified Data.Map.Strict                as M
import           Data.Maybe
import           Data.Typeable
import           Data.Void
import           Debug.Trace
import qualified Penrose.Element                as D
import           Penrose.Env
import           Penrose.Util
import           System.Environment
import           System.IO
import           System.Process
import           System.Random
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Show.Pretty

-- | 'SubOut' is the output of the Substance compiler, comprised of:
-- * Substance AST
-- * (Variable environment, Substance environment)
-- * A mapping from Substance ids to their coresponding labels
data SubOut =
  SubOut SubProg
         (VarEnv, SubEnv)
         LabelMap

instance Show SubOut where
  show (SubOut subProg (subEnv, eqEnv) labelMap) =
    "Parsed Substance program:\n" ++
    ppShow subProg ++
    "\nSubstance type env:\n" ++
    ppShow subEnv ++
    "\nSubstance dyn env:\n" ++
    ppShow eqEnv ++ "\nLabel mappings:\n" ++ ppShow labelMap

--------------------------------------------------------------------------------
-- Substance AST
-- | Program is a sequence of statements
type SubProg = [SubStmt]

-- | Special construct added for "declare and bind" statements, which get parsed into a declare and then a bind in the Substance core language
data SubStmt'
  = DeclBind T
             Var
             Expr
  | DeclList T
             [Var]
  | CoreStmt SubStmt

-- | A Substance statement
data SubStmt
  = Decl T
         Var
  | Bind Var
         Expr
  | EqualE Expr
           Expr
  | EqualQ Predicate
           Predicate
  | ApplyP Predicate
  | LabelDecl Var
              String
  | AutoLabel LabelOption
  | NoLabel [Var]
  deriving (Show, Eq, Typeable)

data LabelOption
  = Default
  | IDs [Var]
  deriving (Show, Eq, Typeable)

------------------------------------
-- | Declaration of Substance objects
data SubDecl =
  SubDeclConst T
               Var
  deriving (Show, Eq, Typeable)

-- | Declaration of Substance constaints
data SubConstr =
  SubConstrConst String
                 [PredArg]
  deriving (Show, Eq, Typeable)

-- | Both declarations and constaints in Substance are regarded as objects,
--   which is possible for Style to select later.
data SubObj
  = LD SubDecl
  | LC SubConstr
  deriving (Show, Eq, Typeable)

newtype ValConstructorName =
  ValConst String
  deriving (Show, Eq, Typeable)

newtype OperatorName =
  OperatorConst String
  deriving (Show, Eq, Typeable)

newtype PredicateName =
  PredicateConst String
  deriving (Show, Eq, Typeable)

type Field = String

data Func = Func
  { nameFunc :: String
  , argFunc  :: [Expr]
  } deriving (Eq, Typeable)

instance Show Func where
  show (Func nameFunc argFunc) = nString ++ "(" ++ aString ++ ")"
    where
      nString = show nameFunc
      aString = show argFunc

data Expr
  = VarE Var
  | ApplyFunc Func
  | ApplyValCons Func
  | DeconstructorE Deconstructor
  | StringLit String
  deriving (Show, Eq, Typeable)

data Deconstructor = Deconstructor
  { varDeconstructor   :: Var
  , fieldDeconstructor :: Field
  } deriving (Show, Eq, Typeable)

data PredArg
  = PE Expr
  | PP Predicate
  deriving (Show, Eq, Typeable)

data Predicate = Predicate
  { predicateName :: PredicateName
  , predicateArgs :: [PredArg]
  , predicatePos  :: SourcePos
  } deriving (Typeable)

-- Equality function that doesn't compare SourcePos
-- TODO: use correct equality comparison in typechecker?
instance Eq Predicate where
  p1 == p2 =
    predicateName p1 == predicateName p2 && predicateArgs p1 == predicateArgs p2

instance Show Predicate where
  show (Predicate predicateName predicateArgs pos) =
    nString ++ "(" ++ aString ++ ")"
    where
      nString = show predicateName
      aString = show predicateArgs

-- | 'substanceParser' is the top-level parser function. The parser contains a list of functions
--    that parse small parts of the language. When parsing a source program, these functions are invoked in a top-down manner.
substanceParser :: VarEnv -> BaseParser [SubStmt']
substanceParser env = evalStateT parser $ Just env
  where
    parser = between scn eof subProg' -- Parse all the statemnts between the spaces to the end of the input file

subProg' :: Parser [SubStmt']
subProg' = subStmt' `sepEndBy` newline'

subStmt', declBind, declList :: Parser SubStmt'
-- NOTE: order matters here. We first parse label statements _and then_ decl list to avoid consuming regular decls
subStmt' =
  tryChoice [declBind, CoreStmt <$> labelStmt, declList, CoreStmt <$> subStmt]

declList = DeclList <$> tParser <*> var `sepBy2` comma

declBind = do
  t <- tParser
  v <- var
  rword ":="
  DeclBind t v <$> expr

subStmt, labelStmt, decl, bind, applyP, labelDecl, autoLabel, noLabel ::
     Parser SubStmt
subStmt = tryChoice [equalE, equalQ, bind, decl, applyP]

labelStmt = tryChoice [labelDecl, autoLabel, noLabel]

decl = Decl <$> tParser <*> var

bind = do
  v' <- var
  rword ":="
  Bind v' <$> expr

equalE = do
  e1 <- expr
  eq
  EqualE e1 <$> expr

equalQ = do
  q1 <- predicate
  rword "<->"
  EqualQ q1 <$> predicate

applyP = ApplyP <$> predicate

labelDecl = do
  rword "Label"
  i <- identifier
  LabelDecl (VarConst i) <$> texExpr

noLabel = rword "NoLabel" >> NoLabel <$> ids
  where
    ids = map VarConst <$> identifier `sepBy1` comma

autoLabel = rword "AutoLabel" >> AutoLabel <$> (defaultLabels <|> idList)
  where
    idList = IDs . map VarConst <$> identifier `sepBy1` comma
    defaultLabels = Default <$ rword "All"

function :: Parser Func
function = do
  n <- identifier
  args <- parens (expr `sepBy1` comma)
  return Func {nameFunc = n, argFunc = args}

field :: Parser Field
field = identifier

expr, valConsOrFunc, deconstructorE :: Parser Expr
expr =
  tryChoice
    [ deconstructorE
    , valConsOrFunc
    , VarE <$> var
    , StringLit <$> doubleQuotedString
    ]

deconstructorE = do
  v <- var
  dot
  f <- field
  return
    (DeconstructorE Deconstructor {varDeconstructor = v, fieldDeconstructor = f})

-- TODO: should this context-based parsing be a general approach throughout?
-- TODO: the new convention is now uppercase for constructors and lowercase for operators
valConsOrFunc = do
  n <- identifier
  e <- get
  let env =
        fromMaybe
          (error "Substance parser: variable environment is not intiialized.")
          e
  args <- parens (expr `sepBy1` comma)
  case (M.lookup n $ valConstructors env, M.lookup n $ operators env)
        -- the id is a value constructor
        of
    (Just _, Nothing) ->
      return (ApplyValCons Func {nameFunc = n, argFunc = args})
        -- the id is an operator
    (Nothing, Just _) -> return (ApplyFunc Func {nameFunc = n, argFunc = args})
    (Nothing, Nothing) -> substanceErr $ "undefined identifier " ++ n
    _ ->
      substanceErr $ n ++ " cannot be both a value constructor and an operator"
  where
    substanceErr s = customFailure (SubstanceError s)

predicate :: Parser Predicate
predicate = do
  n <- identifier
  args <- parens (predicateArg `sepBy1` comma)
  pos <- getSourcePos
  return
    Predicate
    {predicateName = PredicateConst n, predicateArgs = args, predicatePos = pos}

-- NOTE: ordering matters here because expr might succeed first by parsing the predicate name as a var. In general, it's not a good idea to start any parser with an id that's not unique to a type. Fix this later when rewriting the whole thing.
predicateArg :: Parser PredArg
predicateArg = tryChoice [PP <$> predicate, PE <$> expr]

----------------------------------- Utility functions ------------------------------------------
-- | Convert prelude statemnts from the .dsl file into declaration Statements in the Substance program AST
addPrelude :: SubProg -> VarEnv -> SubProg
addPrelude subProg varEnv = map toDecl (preludes varEnv) ++ subProg
  where
    toDecl (v, t) = Decl t v

----------------------------------------- Substance Typechecker ---------------------------
-- | 'check' is the top level function for checking a substance program which calls checkSubStmt on each statement in the
-- program and returnsan updated context from the statement check.
-- Errors are accumulated in the context during checking as they occur.
check :: SubProg -> VarEnv -> VarEnv
check p varEnv =
  let env = foldl checkSubStmt varEnv p
  in if null (errors env)
       then env
       else error $
            "Substance type checking failed with the following problems: \n" ++
            errors env

-- | Statements are checked differently depending on if they are a variable declaration, variable assignment, or predicate statement.
-- Variable declaration statements call checkT to check that the type in the statement is well-formed.
-- The context is updated with errors and the declared variable.
-- A variable assignment statement calls checkVarE and checkExpression to check both the variable and expression in the statement for well-typedness.
-- These functions return a Maybe type of the variable or expression and a string of errors (which may be empty).
-- The error strings are added to the context and the Maybe types are checked for “non-null”
-- values and then equivalence (extra error added to context if the types are not the same for the variable and expression in the statement).
-- Predicate statements are checked by checkPredicate and return a context updated with errors from that checking (if they occur).
checkSubStmt :: VarEnv -> SubStmt -> VarEnv
checkSubStmt varEnv (Decl t (VarConst n)) =
  let env = checkT varEnv t
      env1 = addDeclaredName n env
  in env1 {varMap = M.insert (VarConst n) t $ varMap env1}
checkSubStmt varEnv (Bind v e) =
  let (vstr, vt) = checkVarE varEnv v
      (estr, et) = checkExpression varEnv e -- TODO: Check lazy evaluation on et
  in if isJust vt && isJust et && vt /= et
       then varEnv
            { errors =
                errors varEnv ++
                vstr ++
                estr ++
                "Expression of type " ++
                show et ++ " assigned to variable of type " ++ show vt ++ "\n"
            }
       else varEnv {errors = errors varEnv ++ vstr ++ estr}
checkSubStmt varEnv (EqualE expr1 expr2) =
  let (estr1, et1) = checkExpression varEnv expr1
      (estr2, et2) = checkExpression varEnv expr2
  in if isJust et1 && isJust et2 && et1 /= et2
       then varEnv
            { errors =
                errors varEnv ++
                estr1 ++
                estr2 ++
                "Expression of type " ++
                show et1 ++
                " attempeted to be equal to expression from type" ++
                show et2 ++ "\n"
            }
       else varEnv {errors = errors varEnv ++ estr1 ++ estr2}
checkSubStmt varEnv (EqualQ q1 q2) =
  let env1 = checkPredicate varEnv q1
      env2 = checkPredicate env1 q2
  in env2
checkSubStmt varEnv (ApplyP p) = checkPredicate varEnv p
checkSubStmt varEnv (AutoLabel (IDs vs)) =
  let es = concatMap (fst . checkVarE varEnv) vs
  in varEnv {errors = errors varEnv ++ es}
checkSubStmt varEnv (AutoLabel _) = varEnv -- no checking required
checkSubStmt varEnv (NoLabel ids) =
  let es = concatMap (fst . checkVarE varEnv) ids
  in varEnv {errors = errors varEnv ++ es}
checkSubStmt varEnv (LabelDecl i t) =
  let e = (fst . checkVarE varEnv) i
  in varEnv {errors = errors varEnv ++ e}

-- | The predicate is looked up in the context; if the context doesn’t contain the predicate, then an error is added to the
-- context, otherwise it is checked differently depending on if it takes expressions or other predicates as arguments by
-- calling checkVarPred or checkRecursePred respectively. Any errors found within those checking functions will be accumulated
-- in the context returned by those functions and ultimately this function.
checkPredicate :: VarEnv -> Predicate -> VarEnv
checkPredicate varEnv (Predicate (PredicateConst p) args pos) =
  case checkAndGet p (predicates varEnv) pos of
    Right p ->
      case p of
        Pred1 p1 -> checkVarPred varEnv args p1
        Pred2 p2 -> checkRecursePred varEnv args
    Left err -> varEnv {errors = errors varEnv ++ err}

areAllArgTypes = foldl (\b at1 -> b && isJust at1) True

-- First, this function ensures all the supplied predicate arguments are in fact expressions using isVarPredicate.
-- If they are not, then an execution stopping error is thrown (the error is not in the checking of the program)
-- These expressions are checked by checkExpression for well-typedness returning a list of error strings and Maybe types.
-- The Maybe types list is checked for all “non-null” types.
-- If even one type is “null”, then there were checking failures and the error string is added to the context.
-- Otherwise, the error string is empty and type substitution “sigma” is generated from calling the substitution
-- function on the predicate argument types and formal types stored in the context.
-- The substitution need not be applied to any types for predicates, because using the argument types to create the
-- substitution ensures the argument types match the substitution applied to each formal type.
checkVarPred :: VarEnv -> [PredArg] -> Predicate1 -> VarEnv
checkVarPred varEnv args (Prd1 name yls kls tls) =
  let exprArgs = map isVarPredicate args
      errAndTypesLs = map (checkExpression varEnv) exprArgs
      errls = firsts errAndTypesLs
      err = concat errls
      argTypes = seconds errAndTypesLs
  in if areAllArgTypes argTypes
       then let argTypes2 = map (KT . fromJust) argTypes
                tls2 = map KT tls
                (sigma, substErr) = subst varEnv M.empty argTypes2 tls2
            in varEnv {errors = errors varEnv ++ err ++ substErr} -- err should be empty str
       else varEnv {errors = errors varEnv ++ err}

checkVarOperator :: VarEnv -> [PredArg] -> Operator -> VarEnv
checkVarOperator varEnv args (Operator name yls kls tls _) =
  let exprArgs = map isVarPredicate args
      errAndTypesLs = map (checkExpression varEnv) exprArgs
      errls = firsts errAndTypesLs
      err = concat errls
      argTypes = seconds errAndTypesLs
  in if areAllArgTypes argTypes
       then let argTypes2 = map (KT . fromJust) argTypes
                tls2 = map KT tls
                (sigma, substErr) = subst varEnv M.empty argTypes2 tls2
            in if sigma == M.empty
                 then varEnv {errors = errors varEnv ++ err ++ substErr} -- err should be empty str
                 else varEnv {errors = errors varEnv ++ err ++ substErr} -- err should be empty str
       else varEnv {errors = errors varEnv ++ err}

-- Helper function to determine if predicate arguments are all expressions.
-- It will stop execution if a supplied predicate argument to the function is not an expression.
isVarPredicate :: PredArg -> Expr
isVarPredicate (PP p) = error "Mixed predicate types!"
isVarPredicate (PE p) = p

-- Helper function to determine if predicate arguments are all predicates.
-- It will stop execution if a supplied predicate argument to the function is not a predicate.
isRecursedPredicate :: PredArg -> Predicate
isRecursedPredicate (PP p) = p
isRecursedPredicate (PE p) = error "Mixed predicate types!"

-- This function, first, ensures all the supplied predicate arguments are predicates.
-- It calls checkPredicate (recursively) on each argument predicate returning the context with any accumulated errors found
-- when checking each argument predicate for well-formedness (if there are any).
checkRecursePred :: VarEnv -> [PredArg] -> VarEnv
checkRecursePred varEnv args =
  let predArgs = map isRecursedPredicate args
  in foldl checkPredicate varEnv predArgs

-- This function checks expressions for well-typedness and does it differently for variables or functions/value constructors
-- calling checkVarE and checkFunc respectively for each case.]
-- If errors were found during checking then they are accumulated and returned in a tuple with the Maybe type for the expression.
checkExpression :: VarEnv -> Expr -> (String, Maybe T)
checkExpression varEnv (VarE v) = checkVarE varEnv v
checkExpression varEnv (ApplyFunc f) = checkFunc varEnv f
checkExpression varEnv (ApplyValCons f) = checkFunc varEnv f
checkExpression varEnv (DeconstructorE d) --checkVarE varEnv (varDeconstructor d)
 =
  let (err, t) = checkVarE varEnv (varDeconstructor d)
  in case t of
       Just t' -> checkField varEnv (fieldDeconstructor d) t'
       Nothing -> (err, Nothing)
checkExpression varEnv (StringLit v) = ("", Nothing)

-- Type checking for fields in value deconstructor, check that there is a
-- matched value deconstructor with a matching field a retrieve the type,
-- otherwise, return an error
checkField :: VarEnv -> Field -> T -> (String, Maybe T)
checkField varEnv f t =
  case M.lookup t (typeValConstructor varEnv) of
    Nothing ->
      ("No matching value constructor for the type " ++ show t, Nothing)
    Just v ->
      let m = M.fromList (zip (nsvc v) (tlsvc v))
      in case M.lookup (VarConst f) m of
           Nothing ->
             ( "No matching field " ++
               show f ++ " In the value constructor of " ++ show t
             , Nothing)
           Just t' -> ("", Just t')

-- Checking a variable expression for well-typedness involves looking it up in the context.
-- If it cannot be found in the context, then a tuple is returned of a non-empty error string warning of this problem and
-- a “null” type. Otherwise, a tuple of an empty string and “non-null” type for the variable from the context is returned.
checkVarE :: VarEnv -> Var -> (String, Maybe T)
checkVarE varEnv v =
  case M.lookup v (varMap varEnv) of
    Nothing -> ("Variable " ++ show v ++ " not in environment\n", Nothing)
    vt      -> ("", vt)

--  Looks up the operator or value-constructor in the context. If it cannot be found in the context,
-- then a tuple is returned of a non-empty error string warning of this problem and a “null” type.
-- Otherwise, a tuple of an error string and Maybe type is returned from calls to checkVarConsInEnv and checkFuncInEnv
-- depending on if the Func supplied to this function is an value constructor or operator.
checkFunc :: VarEnv -> Func -> (String, Maybe T)
checkFunc varEnv (Func f args) =
  let vcEnv = M.lookup f (valConstructors varEnv)
      fEnv = M.lookup f (operators varEnv)
  in if isNothing vcEnv && isNothing fEnv
       then ( "Function or Val Constructor " ++
              show f ++ " not in environment\n"
            , Nothing)
       else maybe
              (checkFuncInEnv varEnv (Func f args) (fromJust fEnv))
              (checkVarConsInEnv varEnv (Func f args))
              vcEnv

-- Operates very similarly to checkVarPred described above.
-- The only differences are that this function operates on operators (so checking of arguments to be expressions is
-- unnecessary due to operator parsing) and returns a tuple of an error string and Maybe type.
-- If the substitution “sigma” is generate, then if it is empty, a tuple of an empty error string and the formal
-- return type of the operator is returned, otherwise (if it is not empty) a tuple of an empty error string and the
-- substituted formal return type of the operator is returned. If checking failed for any of the arguments of the operator,
-- then “sigma” is not generated and a tuple of a non-empty error string and “null” type is returned.
checkFuncInEnv :: VarEnv -> Func -> Operator -> (String, Maybe T)
checkFuncInEnv varEnv (Func f args) (Operator name yls kls tls t) =
  let errAndTypesLs = map (checkExpression varEnv) args
      errls = firsts errAndTypesLs
      err = concat errls
      argTypes = map snd errAndTypesLs
  in if foldl (\b at1 -> b && isJust at1) True argTypes
       then let argTypes2 = map (KT . fromJust) argTypes
                tls2 = map KT tls
                (sigma, substErr) = subst varEnv M.empty argTypes2 tls2
            in if sigma == M.empty
                 then (substErr ++ err, Just t) -- err should be empty str
                 else (substErr ++ err, Just (applySubst sigma t)) -- err should be empty str
       else (err, Nothing)

-- Operates exactly the same as checkFuncInEnv above it just operates over value constructors instead of operators.
checkVarConsInEnv :: VarEnv -> Func -> ValConstructor -> (String, Maybe T)
checkVarConsInEnv varEnv (Func f args) (ValConstructor name yls kls nls tls t) =
  let errAndTypesLs = map (checkExpression varEnv) args
      errls = map fst errAndTypesLs
      err = concat errls
      argTypes = map snd errAndTypesLs
  in if foldl (\b at1 -> b && isJust at1) True argTypes
       then let argTypes2 = map (KT . fromJust) argTypes
                tls2 = map KT tls
                (sigma, substErr) = subst varEnv M.empty argTypes2 tls2
            in if sigma == M.empty
                 then (substErr ++ err, Just t) -- err should be empty str
                 else (substErr ++ err, Just (applySubst sigma t)) -- err should be empty str
       else (err, Nothing)

-- Takes a substitution “sigma” and applies it to a type. Types that are single type variables are mapped to their corresponding
-- type which exists in “sigma”. Types that are type constructors are mapped to the same type but with their arguments
-- substituted by “sigma” using applySubstitutionHelper.
applySubst :: M.Map Y Arg -> T -> T
applySubst sigma (TTypeVar vt) =
  case sigma M.! TypeVarY vt of
    AVar v ->
      error
        "Type var being mapped to variable in subst sigma, error in the TypeChecker!"
    AT t -> t
applySubst sigma (TConstr (TypeCtorApp t args pos)) =
  let argsSub = map (applySubstHelper sigma) args
  in TConstr (TypeCtorApp t argsSub pos)

-- This is a helper function which applies a substitution “sigma” to an argument of a type constructor.
-- If the argument is a variable, then it is mapped to its corresponding variable which exists in “sigma”.
-- If the argument is a type, then it is mapped to the “sigma” substitution of itself using a recursive call to applySubstitution
applySubstHelper :: M.Map Y Arg -> Arg -> Arg
applySubstHelper sigma (AVar v) =
  case sigma M.! VarY v of
    res@(AVar v2) -> res
    AT t ->
      error
        "Var being mapped to a type in subst sigma, error in the TypeChecker!"
applySubstHelper sigma (AT t) = AT (applySubst sigma t)

-- This function (along with its helper functions) follows a recursive-descent unification algorithm to find a substitution
-- “sigma” for two type lists. It generates an entry in a substitution map “sigma” whenever a list of argument types (from
-- a Substance program) and its corresponding list of formal types (from the context) differ.
-- All entries in “sigma” must be consistent for it to be a valid substitution.
-- substitutionHelper is called on each element of a list of tuples of corresponding argument and formal types to generate
-- entries in a substitution “sigma”.
subst :: VarEnv -> M.Map Y Arg -> [K] -> [K] -> (M.Map Y Arg, String)
subst varEnv sigma argTypes formalTypes =
  let types = zip argTypes formalTypes
      sigma2 = foldl (substHelper varEnv) sigma types
  in if length argTypes /= length formalTypes
       then ( sigma2
            , "Arguments list lengths are not equal, expected " ++
              show (length formalTypes) ++
              " arguments but call was with " ++
              show (length argTypes) ++ " arguments \n")
       else if compareTypesList varEnv argTypes formalTypes --argTypes /= formalTypes
              then ( sigma2
                   , "Incorrect types of arguments, expected " ++
                     show formalTypes ++ " , but was " ++ show argTypes ++ " \n")
              else (sigma2, "")

compareTypesList :: VarEnv -> [K] -> [K] -> Bool
compareTypesList varEnv argTypes formalTypes =
  let u = zip argTypes formalTypes
      f = filter (compareTypes varEnv) u
  in length u /= length f

compareTypes :: VarEnv -> (K, K) -> Bool
compareTypes varEnv (k1, k2) = (k1 == k2 || isSubtypeK k1 k2 varEnv) -- TODO: remove the equality here (or derive Eq to not include SourcePos)

-- Ensures an argument type and formal type matches where they should match, otherwise a runtime error is generated.
-- In places where they do not need to match exactly (where type and regular variables exist in the formal type)
-- a substitution entry is generated. substitutionHelper2 helps in generating these entries for type constructor arguments and
-- substitutionInsert does the insertion of the entry into the substitution map “sigma”.
substHelper varEnv sigma (KT (TConstr (TypeCtorApp atc argsAT pos1)), KT (TConstr (TypeCtorApp ftc argsFT pos2)))
  | atc `elem` declaredNames varEnv || ftc `elem` declaredNames varEnv =
    substHelper2 varEnv sigma (AVar (VarConst atc), AVar (VarConst ftc))
  | atc /= ftc &&
      not
        (isSubtype
           (TConstr (TypeCtorApp atc argsAT pos1))
           (TConstr (TypeCtorApp ftc argsFT pos2))
           varEnv) =
    error
      ("Argument type " ++
       show atc ++ " doesn't match expected type " ++ show ftc)
  | otherwise =
    let args = zip argsAT argsFT
        sigma2 = foldl (substHelper2 varEnv) sigma args
    in sigma2
substHelper varEnv sigma (Ktype aT, KT fT) =
  error
    ("Argument type " ++ show aT ++ " doesn't match expected type " ++ show fT)
substHelper varEnv sigma (KT (TTypeVar atv), KT (TConstr (TypeCtorApp ftc argsFT pos))) =
  error
    ("Argument type " ++ show atv ++ " doesn't match expected type " ++ show ftc)
substHelper varEnv sigma (KT aT, Ktype fT) =
  error
    ("Argument type " ++ show aT ++ " doesn't match expected type " ++ show fT)

-- This helper function makes sure an argument type’s argument matches a formal type’s argument where they should match,
-- otherwise a runtime error is generated. In places where they do not need to match exactly
-- (where type and regular variables exist in the formal type’s argument), a substitution entry is generated and inserted
-- into the substitution map “sigma” using substitutionInsert. Note that substitutionHelper is called recursively to handle
-- substitutions for an argument type’s argument and corresponding formal type’s argument that are both types themselves.
substHelper2 :: VarEnv -> M.Map Y Arg -> (Arg, Arg) -> M.Map Y Arg
substHelper2 varEnv sigma (AVar av, AVar fv) =
  substInsert sigma (VarY fv) (AVar av)
substHelper2 varEnv sigma (AT at, AT ft) =
  substHelper varEnv sigma (KT at, KT ft)
substHelper2 varEnv sigma (AVar av, AT ft) =
  error
    ("Argument type's argument " ++
     show av ++ " doesn't match expected type's argument " ++ show ft)
substHelper2 varEnv sigma (AT at, AVar fv) =
  error
    ("Argument type's argument " ++
     show at ++ " doesn't match expected type's argument " ++ show fv)

-- Handles the consistency of entries in the substitution “sigma”, by ensuring that if an entry being inserted into “sigma”
-- already exists in “sigma” it is the same entry as the one already in “sigma”.
-- If the entry doesn’t already exist in “sigma”, then it can be inserted directly without a check for consistency.
substInsert :: M.Map Y Arg -> Y -> Arg -> M.Map Y Arg
substInsert sigma y arg =
  case M.lookup y sigma of
    Nothing -> M.insert y arg sigma
    arg' ->
      if arg /= fromJust arg'
        then error "Substitutions inconsistent - no subst can exist"
        else sigma

--argTypeLookupHelper :: VarEnv -> Y -> Either K String
--argTypeLookupHelper varEnv (TypeVarY t) = case M.lookup t (typeVarMap varEnv) of
--                                   Nothing -> Right "Argument " ++ (show t) ++ " not in environment\n"
--                                   tType -> Left tType
--argTypeLookupHelper varEnv (VarY v) = case M.lookup v (varMap varEnv) of
--                               Nothing -> Right "Argument " ++ (show v) ++ " not in environment\n"
--                               vType -> Left vType
----------------------------------------- Binding & Equality Environment ------------------
-- | Definition of the Substance environment + helper functions.
--   Contains binding information and equality of expressions and predicates
--   In order to calculate all the equalities, we compute the closure of the
--   equalities in Substance + symmetry.
-- The equalities do NOT contain self-equalities, which are manually checked by the Style matcher
data SubEnv = SubEnv
  { exprEqualities :: [(Expr, Expr)]
  , predEqualities :: [(Predicate, Predicate)]
  , bindings       :: M.Map Var Expr
  , subPreds       :: [Predicate]
  } deriving (Show, Eq, Typeable)

-- | The top level function for computing the Substance environement
--   Important: this function assumes it runs after the typechecker and that
--              the program is well-formed (as well as the DSLL)
loadSubEnv :: SubProg -> SubEnv
loadSubEnv p =
  let subEnv1 = foldl loadStatement initE p
      subEnv2 = computeEqualityClosure subEnv1
  in subEnv2
  where
    initE =
      SubEnv
      { exprEqualities = []
      , predEqualities = []
      , bindings = M.empty
      , subPreds = []
      }

-- | The order in all the lists is reserved
loadStatement :: SubEnv -> SubStmt -> SubEnv
loadStatement e (EqualE expr1 expr2) =
  e {exprEqualities = (expr1, expr2) : exprEqualities e}
loadStatement e (EqualQ q1 q2) =
  e {predEqualities = (q1, q2) : predEqualities e}
loadStatement e (Bind v expr) = e {bindings = M.insert v expr $ bindings e}
loadStatement e (ApplyP p) = e {subPreds = p : subPreds e}
loadStatement e _ = e -- for all other statements, simply pass on the environment

computeEqualityClosure :: SubEnv -> SubEnv
computeEqualityClosure e =
  e
  { predEqualities = transitiveClosure (predEqualities e)
  , exprEqualities = transitiveClosure (exprEqualities e)
  }

-- | Given an environment and 2 expressions, determine whether those expressions are equal
--   For use in Style
exprsDeclaredEqual :: SubEnv -> Expr -> Expr -> Bool
exprsDeclaredEqual env e1 e2 =
  (e1, e2) `elem` exprEqualities env || (e2, e1) `elem` exprEqualities env

-- | Given an environment and 2 predicates determine whether those predicates are equal
--   For use in Style
predsDeclaredEqual :: SubEnv -> Predicate -> Predicate -> Bool
predsDeclaredEqual env q1 q2 =
  (q1, q2) `elem` predEqualities env || (q2, q1) `elem` predEqualities env

-- --------------------------------------- Substance Loader --------------------------------
-- | Load all the Substance objects for visualization in Runtime.hs
-- | Mapping from Subtance IDs to (maybe) label texts. A Substance object
-- might not have labels due to the lack of @Label@ or existance of
-- @NoLabel@
type LabelMap = M.Map String (Maybe String)

-- TODO: better name for the type
data SubObjects = SubObjects
    -- | declared Substance objects (including constraints, which is, again, viewed also as objects)
  { subObjs   :: [SubObj]
    -- | a map that stores all label texts associated with each Substance
    -- object (TODO: are predicates included? currently not.)
  , subLabels :: LabelMap
  } deriving (Show, Eq, Typeable)

-- | generate a mapping from substance IDs to their label strings
getLabelMap :: SubProg -> VarEnv -> LabelMap
getLabelMap p env = collectLabels subIds p
  where
    subIds = map (\(VarConst v) -> v) $ M.keys (varMap env)

-- | Given all label statements and Substance IDs, generate a map from
-- all ids to their labels
collectLabels :: [String] -> SubProg -> LabelMap
collectLabels ids =
  foldl
    (\m stmt ->
       case stmt of
         LabelDecl (VarConst i) s -> M.insert i (Just s) m
         AutoLabel Default -> M.fromList $ zip ids $ map Just ids
         AutoLabel (IDs ids) ->
           foldl (\m' (VarConst i) -> M.insert i (Just i) m') m ids
         NoLabel ids -> foldl (\m' (VarConst i) -> M.insert i Nothing m') m ids
         _ -> m)
    initmap
  where
    initmap = M.fromList $ map (\i -> (i, Nothing)) ids

-- | Convert a list of sugared Substance statements to core statements
toSubCore :: [SubStmt'] -> [SubStmt]
toSubCore = concatMap toCoreStmt
    -- Convert a "declare and bind" to a "declare, then bind" as separate statements
  where
    toCoreStmt (DeclBind t v e) = [Decl t v, Bind v e]
    -- Convert a list of declarations to individual ones
    toCoreStmt (DeclList t vs)  = map (Decl t) vs
    toCoreStmt (CoreStmt s)     = [s]

-- | 'parseSubstance' runs the actual parser function: 'substanceParser', taking in a program String, parses it, semantically checks it, and eventually invoke Alloy if needed. It outputs a collection of Substance objects at the end.
parseSubstance :: String -> String -> VarEnv -> Either CompilerError SubOut
parseSubstance subFile subIn varEnv =
  case runParser (substanceParser varEnv) subFile subIn of
    Left err -> Left $ SubstanceParse (errorBundlePretty err)
    Right subProg -> do
      let subProg' = addPrelude (toSubCore subProg) varEnv
      let subTypeEnv = check subProg' varEnv
      let subDynEnv = loadSubEnv subProg'
      let labelMap = getLabelMap subProg' subTypeEnv
      Right $ SubOut subProg' (subTypeEnv, subDynEnv) labelMap
-- --------------------------------------- Test Driver -------------------------
-- | For testing: run this function in GHCI
-- testSubstance :: String -> String -> IO ()
-- testSubstance subFile elmFile = do
--   subIn <- readFile subFile
--   elmIn <- readFile elmFile
--   case parseElement "" elmIn of
--     Left e -> error $ show e
--     Right env ->
--       case parseSubstance "" subIn env of
--         Left e -> error $ show e
--         Right (SubOut prog _ _) -> do
--           print prog
--           print $ prettySubstance prog
--   return ()
