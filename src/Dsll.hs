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
     deriving (Show, Eq, Typeable)

data StringLit = StringLitConst String
     deriving (Show, Eq, Typeable)

data Var = VarConst String
     deriving (Show, Eq, Typeable)

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
data Cd = Cd{nameCd :: String, inputCd::[(StringLit,K)], outputCd::Type}
    deriving (Eq, Typeable)

instance Show Cd where
    show (Cd nameCd inputCd outputCd) = "(TCon, " ++ nString ++ ", ValOfType " ++ iString ++ ", Output " ++ oString ++")"
        where nString = show nameCd
              iString = show inputCd
              oString = show outputCd


-- | vconstructor
data Vd = Vd{ nameVd :: String, varsVd :: [(Y,K)], typesVd :: [(StringLit,T)], toVd:: T}
    deriving (Eq, Typeable)

instance Show Vd where
    show (Vd nameVd varsVd typesVd toVd) =
     "(VCon, " ++ aString ++ ", forvars " ++ bString ++ ", fortypes " ++ cString ++ ", outputT " ++ dString ++ ")"
        where aString = show nameVd
              bString = show varsVd
              cString = show typesVd
              dString = show toVd

-- | predicates
data Od = Od{ nameOd :: String, varsOd :: [(Y,K)], typesOd :: [(StringLit,T)], toOd:: T}
    deriving (Eq, Typeable)

instance Show Od where
    show (Od nameOd varsOd typesOd toOd) =
     "(Op, " ++ aString ++ ", forvars " ++ bString ++ ", fortypes " ++ cString ++ ", outputT " ++ dString ++ ")"
        where aString = show nameOd
              bString = show varsOd
              cString = show typesOd
              dString = show toOd

-- | predicates
data Pd = Pd{ namePd :: String, varsPd :: [(Y,K)], typesPd :: [(StringLit,T)], propsPd :: [(StringLit, Prop)], toPd:: Prop}
    deriving (Eq, Typeable)

instance Show Pd where
    show (Pd namePd varsPd typesPd propsPd toPd) =
     "(Pred, " ++ aString ++ ", forvars " ++ bString ++ ", fortypes " ++ cString ++ ", forProps " ++ dString ++ ", outputT " ++ eString ++ ")"
        where aString = show namePd
              bString = show varsPd
              cString = show typesPd
              dString = show propsPd
              eString = show toPd

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

stringLitParser :: Parser StringLit
stringLitParser = do
    i <- identifier
    return (StringLitConst i)


tParser, tConstructorInvokerParser, typeVarParser' :: Parser T
tParser = try tConstructorInvokerParser <|> typeVarParser'
tConstructorInvokerParser = do
    i         <- identifier
    arguments <- parens ( argParser `sepBy1` comma)
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
    b' <- listOut (stringLitParser `sepBy1` comma)
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
btParser :: Parser ([StringLit],[T])
btParser = do
  lparen
  b' <- listOut (stringLitParser `sepBy1` comma)
  colon
  t' <- listOut (tParser `sepBy1` comma)
  rparen
  return (b',t')

-- | for the cases we do not have (b,t) list 
emptybtParser :: Parser ([StringLit],[T])
emptybtParser = return ([],[])


-- | var constructor parser
vdParser :: Parser Vd
vdParser = do
  rword "vconstructor"
  name <- identifier
  (y',k') <- try ykParser <|> emptyykParser
  (b',t') <- try btParser <|> emptybtParser
  colon
  t'' <- tParser
  return Vd{nameVd = name, varsVd = (zip y' k'), typesVd  =  (zip b' t'), toVd = t''}



  -- | operation parser
odParser :: Parser Od
odParser = do
  rword "operator"
  name <- identifier
  (y',k') <- try ykParser <|> emptyykParser
  (b',t') <- try btParser <|> emptybtParser
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
  (b',t') <- try btParser <|> emptybtParser
  colon
  p' <- propParser
  return Pd{namePd = name, varsPd = (zip y' k'), typesPd  =  (zip b' t'), propsPd = [], toPd = p'}
pd2 = do
  rword "predicate"
  name <- identifier
  lparen
  b' <- listOut (stringLitParser `sepBy1` comma)
  colon
  prop' <- listOut (propParser `sepBy1` comma)
  rparen
  colon
  p' <- propParser
  return Pd{namePd = name, varsPd = [], typesPd  =  [], propsPd = (zip b' prop'), toPd = p'}


-- -- --------------------------------------- DSLL Semantic Checker ---------------------------

-- -- -- | Environment for the dsll semantic checker. As the 'check' function executes, it
-- -- -- accumulate information such as symbol tables in the environment.

-- -- -- Specify the possible context elements
-- -- data ContextElement = TypeContext T
-- --              | TContext Var
-- --              | CContext Cd
-- --              | EmptyContext
-- --     deriving(Show, Eq, Typeable)

-- -- -- Currently, the environment contains the context, i.e. a list of context elements
-- -- -- might be extended in the future.
-- -- data DsllEnv = DsllEnv{
-- --   context :: [ContextElement]
-- -- } deriving(Show, Eq, Typeable)

-- -- -- | 'check' is the top-level semantic checking function. It takes a DSLL
-- -- -- program as the input, checks the validity of the program acoording to the typechecking rules, and outputs
-- -- -- a collection of information.

-- -- check :: DSLLProg -> DsllEnv
-- -- check p = let env1  = foldl loadContext initE p
-- --           in env1 { context = reverse $ context env1}
-- --           where initE = DsllEnv {context = [TContext (VarConst "type")]}


-- -- -- | 'loadContext' loads the context of the DSLL program from the given AST representation
-- -- loadContext : DsllEnv -> 


-- --------------------------------------- Test Driver -------------------------------------
-- | For testing: first uncomment the module definition to make this module the
-- Main module. Usage: ghc Dsll.hs; ./Dsll <dsll-file> <output-file>

main :: IO ()
main = do
    [dsllFile, outputFile] <- getArgs
    dsllIn <- readFile dsllFile
    parseTest dsllParser dsllIn
    -- case (parse dsllParser dsllFile dsllIn) of
    --     Left err -> putStr (parseErrorPretty err)
    --     Right xs -> writeFile outputFile (show xs)
    -- putStrLn "Parsing Done!"  
    return ()


    






