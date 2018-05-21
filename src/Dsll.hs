-- | "DSLL" contains the grammers and parser for the 
--    DSLL language
--    Author: Dor Ma'ayan, May 2018

{-# OPTIONS_HADDOCK prune #-}
--module DSLL where
module Main (main) where -- for debugging purposes

import Utils
import System.Process
import Control.Monad (void)
import Data.Void
import Debug.Trace
import System.IO -- read/write to file
import System.Environment
import Control.Arrow ((>>>))
import System.Random
import Debug.Trace
import Data.Functor.Classes
import Data.List
import Data.Maybe (fromMaybe)
import Data.Typeable
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
--import Text.PrettyPrint
--import Text.PrettyPrint.HughesPJClass hiding (colon, comma, parens, braces)
import qualified Data.Map.Strict as M
import qualified Text.Megaparsec.Char.Lexer as L
--------------------------------------- DSLL AST ---------------------------------------
data TypeVar = TypeVarConst String
     deriving (Show, Eq, Typeable, Ord)

data Var = VarConst String
     deriving (Show, Eq, Typeable, Ord)

data Y = TypeVarY TypeVar | VarY Var
     deriving (Show, Eq, Typeable)

data ConstructorInvoker = ConstructorInvoker { nameCons :: String, argCons:: [Arg]}
    deriving (Eq, Typeable)
instance Show ConstructorInvoker where
    show (ConstructorInvoker nameCons argCons) = nString ++ "(" ++ aString ++ ")"
        where nString = show nameCons
              aString = show argCons

data T = TTypeVar TypeVar
    | TConstr ConstructorInvoker
    deriving (Show, Eq, Typeable)

data Arg = AVar Var
    | AT T
    deriving (Show, Eq, Typeable)

data K = Ktype Type
     | KT T
     deriving (Show, Eq, Typeable)

data Prop = PropConst String
     deriving (Show, Eq, Typeable)

data Type = TypeConst String
     deriving (Show, Eq, Typeable)

-- | tconstructor
data Cd = Cd{nameCd :: String, inputCd::[(Y,K)], outputCd::Type}
    deriving (Eq, Typeable)

instance Show Cd where
    show (Cd nameCd inputCd outputCd) = "(TCon, " ++ nString ++ ", ValOfType " ++ iString ++ ", Output " ++ oString ++")"
        where nString = show nameCd
              iString = show inputCd
              oString = show outputCd


-- | vconstructor
data Vd = Vd{ nameVd :: String, varsVd :: [(Y,K)], typesVd :: [(Var,T)], toVd:: T}
    deriving (Eq, Typeable)

instance Show Vd where
    show (Vd nameVd varsVd typesVd toVd) =
     "(VCon, " ++ aString ++ ", forvars " ++ bString ++ ", fortypes " ++ cString ++ ", outputT " ++ dString ++ ")"
        where aString = show nameVd
              bString = show varsVd
              cString = show typesVd
              dString = show toVd

-- | predicates
data Od = Od{ nameOd :: String, varsOd :: [(Y,K)], typesOd :: [(Var,T)], toOd:: T}
    deriving (Eq, Typeable)

instance Show Od where
    show (Od nameOd varsOd typesOd toOd) =
     "(Op, " ++ aString ++ ", forvars " ++ bString ++ ", fortypes " ++ cString ++ ", outputT " ++ dString ++ ")"
        where aString = show nameOd
              bString = show varsOd
              cString = show typesOd
              dString = show toOd

-- | predicates
data Pd = Pd1Const Pd1 | Pd2Const Pd2 deriving (Show, Eq, Typeable)

data Pd1 = Pd1 { namePd1 :: String, varsPd1 :: [(Y,K)], typesPd1 :: [(Var,T)], toPd1:: Prop}
    deriving (Eq, Typeable)

instance Show Pd1 where
    show (Pd1 namePd1 varsPd1 typesPd1 toPd1) =
     "(Pred, " ++ aString ++ ", forvars " ++ bString ++ ", fortypes " ++ cString ++ ", outputT " ++ dString ++ ")"
        where aString = show namePd1
              bString = show varsPd1
              cString = show typesPd1
              dString = show toPd1

data Pd2 = Pd2 { namePd2 :: String, propsPd2 :: [(Var, Prop)], toPd2:: Prop}
    deriving (Eq, Typeable)

instance Show Pd2 where
    show (Pd2 namePd2 propsPd2 toPd2) =
     "(Pred, " ++ aString ++ ", forProps " ++ bString ++ ", outputT " ++ cString ++ ")"
        where aString = show namePd2
              bString = show propsPd2
              cString = show toPd2


-- | DSLL program composed out of type constructors followed by ar declarations, operations and predicates
data DSLLProg = DSLLProg{cd :: [Cd], vd :: [Vd], od :: [Od], pd :: [Pd]} 
    deriving (Eq, Typeable)

instance Show DSLLProg where
    show (DSLLProg cd vd od pd) = "types = " ++ cdString ++ "\n \n" ++ "vars = " ++ vdString ++ "\n \n" ++ "operations = " ++ odString ++ "\n \n" ++ "predicates = " ++ pdString ++ "\n \n"
        where cdString = show cd
              vdString = show vd
              odString = show od
              pdString = show pd

-- --------------------------------------- DSLL Parser -------------------------------------


-- | 'DSLLParser' is the top-level parser function. The parser contains a list of functions
--    that parse small parts of the language. When parsing a source program, these functions are invoked in a top-down manner.
dsllParser :: Parser DSLLProg
dsllParser = between scn eof dsllProg -- Parse all the statemnts between the spaces to the end of the input file

-- |'dsllProg' parses the entire actual DSLL program which is a collection of constructors followed by a collection of
--   operations followed by a collection of predicates
dsllProg :: Parser DSLLProg
dsllProg = do
    listCd <- cdParser `sepEndBy` newline'
    listVd <- vdParser `sepEndBy` newline'
    listOd <- odParser `sepEndBy` newline'
    listPd <- pdParser `sepEndBy` newline'
    return DSLLProg {cd = listCd, vd = listVd, od =  listOd, pd =  listPd}


typeParser :: Parser Type
typeParser = do
    rword "type"
    return (TypeConst "type")

varParser :: Parser Var
varParser = do
    i <- identifier
    return (VarConst i)

typeVarParser :: Parser TypeVar
typeVarParser = do
    aps
    i <- identifier
    return (TypeVarConst i)

yParser, y1, y2 :: Parser Y
yParser = try y1 <|> y2
y1 = do
  i <- varParser
  return (VarY i)
y2 = do
  i <- typeVarParser
  return (TypeVarY i)

propParser :: Parser Prop
propParser = do
    rword "Prop"
    return (PropConst "Prop")

emptyArgList :: Parser [Arg]
emptyArgList = do
  lparen
  rparen
  return []

tParser, tConstructorInvokerParser, typeVarParser' :: Parser T
tParser = try tConstructorInvokerParser <|> typeVarParser'
tConstructorInvokerParser = do
    i         <- identifier
    arguments <- try (parens (argParser `sepBy1` comma)) <|> emptyArgList
    return (TConstr (ConstructorInvoker {nameCons = i, argCons = arguments}))
typeVarParser' = do
    i <- typeVarParser
    return (TTypeVar i)

argParser, varParser', tParser'  :: Parser Arg
argParser = try tParser' <|> varParser'
varParser' = do
     i <- varParser
     return (AVar i)
tParser' = do
    t <- tParser
    return (AT t)


kParser, kTypeParser, tParser'' :: Parser K
kParser = try kTypeParser <|> try tParser''
kTypeParser = do
     rword "type"
     return (Ktype (TypeConst "type"))
tParser'' = do
    t <- tParser
    return (KT t)

-- | type constructor parser
cdParser, cd1, cd2 :: Parser Cd
cdParser = try cd1 <|> cd2
cd1= do
    rword "tconstructor"
    name <- identifier
    lparen
    b' <- listOut (yParser `sepBy1` comma)
    colon
    k' <- listOut (kParser `sepBy1` comma)
    rparen
    colon
    t' <- typeParser
    return Cd {nameCd = name, inputCd =  (zip b' k'), outputCd =  t'}
cd2 = do
    rword "tconstructor"
    name <- identifier
    colon
    t' <- typeParser
    return Cd {nameCd = name, inputCd =  [], outputCd =  t'}


-- | parser for the (y,k) list, refactored out to prevent duplication
ykParser :: Parser ([Y],[K])
ykParser = do
  slparen
  y' <- listOut (yParser `sepBy1` comma)
  colon
  k' <- listOut (kParser `sepBy1` comma)
  srparen
  return (y',k')

-- | for the cases we do not have (y,k) list 
emptyykParser :: Parser ([Y],[K])
emptyykParser = return ([],[])

-- | parser for the (b,t) list, refactored out to prevent duplication
xtParser :: Parser ([Var],[T])
xtParser = do
  lparen
  x' <- listOut (varParser `sepBy1` comma)
  colon
  t' <- listOut (tParser `sepBy1` comma)
  rparen
  return (x',t')

-- | for the cases we do not have (b,t) list 
emptyxtParser :: Parser ([Var],[T])
emptyxtParser = return ([],[])


-- | var constructor parser
vdParser :: Parser Vd
vdParser = do
  rword "vconstructor"
  name <- identifier
  (y',k') <- try ykParser <|> emptyykParser
  (b',t') <- try xtParser <|> emptyxtParser
  colon
  t'' <- tParser
  return Vd{nameVd = name, varsVd = (zip y' k'), typesVd  =  (zip b' t'), toVd = t''}



  -- | operation parser
odParser :: Parser Od
odParser = do
  rword "operator"
  name <- identifier
  (y',k') <- try ykParser <|> emptyykParser
  (b',t') <- try xtParser <|> emptyxtParser
  colon
  t'' <- tParser
  return Od{nameOd = name, varsOd = (zip y' k'), typesOd  =  (zip b' t'), toOd = t''}


-- | predicate parser
pdParser, pd1, pd2 :: Parser Pd
pdParser = try pd1 <|> pd2 
pd1 = do
  rword "predicate"
  name <- identifier
  (y',k') <- try ykParser <|> emptyykParser
  (b',t') <- try xtParser <|> emptyxtParser
  colon
  p' <- propParser
  return (Pd1Const (Pd1{namePd1 = name, varsPd1 = (zip y' k'), typesPd1  =  (zip b' t'), toPd1 = p'}))
pd2 = do
  rword "predicate"
  name <- identifier
  lparen
  b' <- listOut (varParser `sepBy1` comma)
  colon
  prop' <- listOut (propParser `sepBy1` comma)
  rparen
  colon
  p' <- propParser
  return (Pd2Const (Pd2{namePd2 = name, propsPd2 = (zip b' prop'), toPd2 = p'}))


-- --------------------------------------- DSLL Semantic Checker ---------------------------
-- | Environment for the dsll semantic checker. As the 'check' function executes, it
-- accumulate information such as symbol tables in the environment.

-- | list of elements that might appear in the global context

data Ttype = Ttype {yt :: Y, kt :: K}  deriving (Show, Eq, Typeable)
data TypeConstructor = TypeConstructor {nametc :: String, klstc :: [K], typtc :: Type} deriving (Show, Eq, Typeable)
data VarConstructor = VarConstructor {namevc :: String, ylsvc :: [Y], klsvc :: [K], tlsvc :: [T], tvc :: T} deriving (Show, Eq, Typeable)
data Operation = Operation {nameop :: String, ylsop :: [Y], klsop :: [K], tlsop :: [T], top :: T} deriving (Show, Eq, Typeable)
data Predicate = Pred1 Predicate1 | Pred2 Predicate2 deriving (Show, Eq, Typeable)
data Predicate1 = Predicate1 {namepred1 :: String, ylspred1 :: [Y], klspred1 :: [K], tlspred1 :: [T], ppred1 :: Prop} deriving (Show, Eq, Typeable)
data Predicate2 = Predicate2 {namepred2 :: String, plspred2 :: [Prop], ppred2 :: Prop} deriving (Show, Eq, Typeable)


data DsllEnv = DsllEnv{
  typeContructors :: M.Map String TypeConstructor,
  varConstructors :: M.Map String VarConstructor,
  operations :: M.Map String Operation,
  predicates :: M.Map String Predicate,
  typeVarMap :: M.Map TypeVar Type,
  varMap :: M.Map Var T
  --localEnv = [] -- TODO: Work with an inner env as well
} deriving(Show, Eq, Typeable)


-- | helper functions for the dsll typechecking

firsts :: [(a,b)] -> [a]
firsts xs = [x | (x,_) <- xs]

seconds :: [(a,b)] -> [b]
seconds xs = [x | (_,x) <- xs]

second :: (a,b) -> b
second (a,b) = b

checkAndGet k m = case M.lookup k m of
  Nothing -> error ("Type " ++ k ++ " Doesn't exsist in the context")
  Just v -> v


lookUpK :: DsllEnv -> Arg -> K
lookUpK e (AT  t)  = (Ktype (TypeConst "type")) --error("the env is: " ++ (show e))--(Ktype (TypeConst "type"))
lookUpK e (AVar v) = (KT ((varMap e) M.! v))


getTypesOfArgs :: DsllEnv -> [Arg] -> [K]
getTypesOfArgs e args = map (lookUpK e) args

updateEnv :: DsllEnv -> (Y,K) -> DsllEnv
updateEnv e (TypeVarY y, Ktype t) = e {typeVarMap = M.insert y t $ typeVarMap e}
updateEnv e (VarY y, KT t) = e {varMap = M.insert y t $ varMap e}
updateEnv e err = error("Problem in update: " ++ (show err))


-- | 'check' is the top-level semantic checking function. It takes a DSLL
-- program as the input, checks the validity of the program acoording to the typechecking rules, and outputs
-- a collection of information.
check :: DSLLProg -> DsllEnv
check p =  let env1  = foldl checkTypeConstructors initE (cd p)
               env2  = foldl checkVarConstructors env1 (vd p)
               env3  = foldl checkOperations env2 (od p)
               env4  = foldl checkPredicates env3 (pd p)
           in env4 { typeContructors =  typeContructors env4}
           where initE = DsllEnv {typeContructors = M.empty, varConstructors = M.empty,
            operations = M.empty, predicates = M.empty, typeVarMap = M.empty, varMap = M.empty}

checkTypeVar :: DsllEnv -> TypeVar -> DsllEnv
checkTypeVar e v = if (M.member v (typeVarMap e)) then e
                else error ("TypeVar " ++ (show v) ++ "is not in scope")

checkVar :: DsllEnv -> Var -> DsllEnv
checkVar e v = if (M.member v (varMap e)) then e
                else error ("Var " ++ (show v) ++ "is not in scope")

checkY :: DsllEnv -> Y -> DsllEnv
checkY e (TypeVarY y) = checkTypeVar e y
checkY e (VarY y) = checkVar e y

checkArg :: DsllEnv -> Arg -> DsllEnv
checkArg e (AVar v) = checkVar e v
checkArg e (AT t) = checkT e t

checkT :: DsllEnv -> T -> DsllEnv
checkT e (TTypeVar t) = checkTypeVar e t
checkT e (TConstr c) = checkConstructorInvoker e c

checkType :: DsllEnv -> Type -> DsllEnv
checkType e t = e


checkConstructorInvoker :: DsllEnv -> ConstructorInvoker -> DsllEnv
checkConstructorInvoker e const = let name = (nameCons const)
                                      args = (argCons const)
                                      env1 = foldl checkArg e args
                                      kls1 = getTypesOfArgs e args 
                                      kls2 = klstc (checkAndGet name (typeContructors e))
                                      in if kls1 /= kls2 then error("Args does not match: " ++ (show kls1) ++ " != " ++ (show kls2)) else env1

checkK :: DsllEnv -> K -> DsllEnv
checkK e (Ktype t) = (checkType e t)
checkK e (KT t) = (checkT e t)


checkTypeConstructors :: DsllEnv -> Cd -> DsllEnv
checkTypeConstructors e c = let kls = (seconds (inputCd c))
                                env1 = foldl checkK e kls
                                tc = TypeConstructor {nametc = nameCd c, klstc = (seconds (inputCd c))
                                                       , typtc = outputCd c}
                        in env1 {typeContructors = M.insert (nameCd c) tc $ typeContructors env1 }


checkVarConstructors :: DsllEnv -> Vd -> DsllEnv
checkVarConstructors e v = let kls = (seconds (varsVd v))
                               env1 = foldl checkK e kls
                               localEnv = foldl updateEnv env1 (varsVd v)
                               args = (seconds (typesVd v))
                               res = (toVd v)
                               env2 = foldl checkT localEnv args
                               temp = checkT localEnv res
                               vc = VarConstructor {namevc = nameVd v,  ylsvc = (firsts (varsVd v))
                                         , klsvc = (seconds (varsVd v)) , tlsvc = (seconds (typesVd v)), tvc = (toVd v)}
                            in if ((env2 == e || env2 /= e) && (temp == e || temp /= e)) then e{varConstructors = M.insert (nameVd v) vc $ varConstructors e} else error ("Error!")


checkOperations :: DsllEnv -> Od -> DsllEnv
checkOperations e v = let kls = (seconds (varsOd v))
                          env1 = foldl checkK e kls
                          localEnv = foldl updateEnv env1 (varsOd v)
                          args = (seconds (typesOd v))
                          res = (toOd v)
                          env2 = foldl checkT localEnv args
                          temp = checkT localEnv res
                          op = Operation {nameop = nameOd v,  ylsop = (firsts (varsOd v))
                                    , klsop = (seconds (varsOd v)) , tlsop = (seconds (typesOd v)), top = (toOd v)}
                        in if ((env2 == e || env2 /= e) && (temp == e || temp /= e)) then e{operations = M.insert (nameOd v) op $ operations e} else error ("Error!")


checkPredicates :: DsllEnv -> Pd -> DsllEnv
checkPredicates e (Pd1Const v) = let kls = (seconds (varsPd1 v))
                                     env1 = foldl checkK e kls
                                     localEnv = foldl updateEnv env1 (varsPd1 v)
                                     args = (seconds (typesPd1 v))
                                     env2 = foldl checkT localEnv args
                                     pd1 = Pred1 Predicate1 {namepred1 = namePd1 v,  ylspred1 = (firsts (varsPd1 v))
                                         , klspred1 = (seconds (varsPd1 v)) , tlspred1 = (seconds (typesPd1 v)), ppred1 = (toPd1 v)}
                                  in if ((env2 == e || env2 /= e)) then e{predicates = M.insert (namePd1 v) pd1 $ predicates e} else error ("Error!")

checkPredicates e (Pd2Const v) = let pd = Pred2 Predicate2 {namepred2 = namePd2 v, plspred2 = (seconds (propsPd2 v)) , ppred2 = (toPd2 v)}
                        in e {predicates = M.insert (namePd2 v) pd $ predicates e }


-- -- | 'loadContext' loads the context of the DSLL program from the given AST representation
-- loadContext : DsllEnv ->  


-- --------------------------------------- Test Driver -------------------------------------
-- | For testing: first uncomment the module definition to make this module the
-- Main module. Usage: ghc Dsll.hs; ./Dsll <dsll-file> <output-file>

main :: IO ()
main = do
  [dsllFile, outputFile] <- getArgs
  dsllIn <- readFile dsllFile
  case (parse dsllParser dsllFile dsllIn) of
    Left err -> putStr (parseErrorPretty err)
    Right xs -> do
      writeFile outputFile (show xs)
      let o = check xs
      putStrLn (show o)
  putStrLn "Parsing Done!"  
  return ()


    






