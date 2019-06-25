-- | "Element" contains the grammers and parser for the
--    Element language
{-# OPTIONS_HADDOCK prune #-}

module Penrose.Element where

import           Control.Arrow                  ((>>>))
import           Control.Monad                  (void)
import           Control.Monad.Combinators.Expr
import           Control.Monad.State.Lazy       (evalStateT)
import           Data.Functor.Classes
import           Data.List
import qualified Data.Map.Strict                as M
import           Data.Maybe                     (fromMaybe)
import           Data.Tuple
import           Data.Typeable
import           Data.Void
import           Debug.Trace
import           Penrose.Env
import qualified Penrose.Tokenizer              as T
import           Penrose.Util
import           System.Environment
import           System.IO
import           System.Process
import           System.Random
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Show.Pretty

--------------------------------------- Element AST -------------------------------
type ElementProg = [ElementStmt]

data ElementStmt
  = CdStmt Cd
  | VdStmt Vd
  | SubtypeDeclStmt SubtypeDecl
  | OdStmt Od
  | PdStmt Pd
  | SnStmt Sn -- Statement notation
  | PreludeDeclStmt Var
                    T
  deriving (Show, Eq, Typeable)

-- | tconstructor
data Cd = Cd
  { nameCd  :: String
  , inputCd :: [(Y, K)]
  } deriving (Eq, Typeable)

instance Show Cd where
  show (Cd nameCd inputCd) =
    "(TCon, " ++ nString ++ ", ValOfType " ++ iString ++ ")"
    where
      nString = show nameCd
      iString = show inputCd

-- | vconstructor
data Vd = Vd
  { nameVd  :: String
  , varsVd  :: [(Y, K)]
  , typesVd :: [(Var, T)]
  , toVd    :: T
  } deriving (Eq, Typeable)

instance Show Vd where
  show (Vd nameVd varsVd typesVd toVd) =
    "(VCon, " ++
    aString ++
    ", forvars " ++
    bString ++ ", fortypes " ++ cString ++ ", outputT " ++ dString ++ ")"
    where
      aString = show nameVd
      bString = show varsVd
      cString = show typesVd
      dString = show toVd

-- | Subtype declarations
data SubtypeDecl = SubtypeDecl
  { subType   :: T
  , superType :: T
  } deriving (Eq, Typeable)

instance Show SubtypeDecl where
  show (SubtypeDecl subType superType) = aString ++ "Subtype of" ++ bString
    where
      aString = show subType
      bString = show superType

-- | predicates
data Od = Od
  { nameOd  :: String
  , varsOd  :: [(Y, K)]
  , typesOd :: [(Var, T)]
  , toOd    :: T
  } deriving (Eq, Typeable)

instance Show Od where
  show (Od nameOd varsOd typesOd toOd) =
    "(Op, " ++
    aString ++
    ", forvars " ++
    bString ++ ", fortypes " ++ cString ++ ", outputT " ++ dString ++ ")"
    where
      aString = show nameOd
      bString = show varsOd
      cString = show typesOd
      dString = show toOd

-- | predicates
data Pd
  = Pd1Const Pd1
  | Pd2Const Pd2
  deriving (Show, Eq, Typeable)

data Pd1 = Pd1
  { namePd1  :: String
  , varsPd1  :: [(Y, K)]
  , typesPd1 :: [(Var, T)]
  } deriving (Eq, Typeable)

instance Show Pd1 where
  show (Pd1 namePd1 varsPd1 typesPd1) =
    "(Pred, " ++
    aString ++
    ", forvars " ++ bString ++ ", fortypes " ++ cString ++ ", outputT " ++ ")"
    where
      aString = show namePd1
      bString = show varsPd1
      cString = show typesPd1

data Pd2 = Pd2
  { namePd2  :: String
  , propsPd2 :: [(Var, Prop)]
  } deriving (Eq, Typeable)

instance Show Pd2 where
  show (Pd2 namePd2 propsPd2) =
    "(Pred, " ++ aString ++ ", forProps " ++ bString ++ ", outputT " ++ ")"
    where
      aString = show namePd2
      bString = show propsPd2

-- | Statement notation (for syntactic sugar)
data Sn = Sn
  { fromSn :: String
  , toSn   :: String
  } deriving (Eq, Typeable)

instance Show Sn where
  show (Sn fromSn toSn) = "(notation: from: " ++ a ++ " to: " ++ b
    where
      a = show fromSn
      b = show toSn

----------------------------------------- Element Parser --------------------------
-- | 'ElementParser' is the top-level parser function. The parser contains a list
-- of functions that parse small parts of the language. When parsing a source
-- program, these functions are invoked in a top-down manner.
-- Parse all the statemnts between the spaces to the end of the input file
elementParser :: BaseParser [ElementStmt]
elementParser = evalStateT elementParser' Nothing

elementParser' :: Parser [ElementStmt]
elementParser' = between scn eof elementProgParser

-- |'elementProg' parses the entire actual Element program which is a collection of
-- constructors followed by a collection of operations followed by a collection
-- of predicates
elementProgParser :: Parser [ElementStmt]
elementProgParser = elementStmt `sepEndBy` newline'

elementStmt :: Parser ElementStmt
elementStmt =
  try snParser <|> try cdParser <|> try vdParser <|> try odParser <|>
  try subtypeDeclParser <|>
  try pdParser <|>
  try preludeParser

-- | type constructor parser
cdParser, cd1, cd2 :: Parser ElementStmt
cdParser = try cd1 <|> cd2

cd1 = do
  rword "type"
  name <- identifier
  (y, k) <- parens ykParser
  pos <- getSourcePos
  return (CdStmt Cd {nameCd = name, inputCd = zip y k})

cd2 = do
  rword "type"
  name <- identifier
  return (CdStmt Cd {nameCd = name, inputCd = []})

-- | sub type declarations parser
subtypeDeclParser :: Parser ElementStmt
subtypeDeclParser = do
  subtype <- tParser
  rword "<:"
  supertype <- tParser
  return $
    SubtypeDeclStmt $ SubtypeDecl {subType = subtype, superType = supertype}

-- | prelude declarations parser
preludeParser :: Parser ElementStmt
preludeParser = do
  rword "value"
  pvar <- varParser
  rword ":"
  ptype <- tParser
  return $ PreludeDeclStmt pvar ptype

-- | parser for the (y,k) list
ykParser :: Parser ([Y], [K])
ykParser = unzip <$> (yWithKind `sepBy1` comma)
  where
    yWithKind = (,) <$> (yParser <* colon) <*> kParser

varWithTypeParser :: Parser (Var, T)
varWithTypeParser = do
  t <- tParser
  v <- option (VarConst "") varParser
  return (v, t)

varWithTypeParserNonOptional :: Parser (Var, T)
varWithTypeParserNonOptional = do
  t <- tParser
  v <- varParser
  return (v, t)

-- | parser for the (b,t) list with optional var names
xtParser :: Parser ([Var], [T])
xtParser = unzip <$> (varWithTypeParser `sepBy1` star)

-- | parser for the (b,t) list with mandatory var names
xtParserNonOptional :: Parser ([Var], [T])
xtParserNonOptional = unzip <$> (varWithTypeParserNonOptional `sepBy1` star)

vpPairParser :: Parser (Var, Prop)
vpPairParser = do
  p <- propParser
  v <- varParser
  return (v, p)

-- | parser for the (x, Prop) list
xPropParser :: Parser ([Var], [Prop])
xPropParser = unzip <$> (vpPairParser `sepBy1` star)

-- | var constructor parser
vdParser :: Parser ElementStmt
vdParser = do
  rword "constructor"
  name <- identifier
  colon
  (y', k') <- option ([], []) $ brackets ykParser
  (b', t') <- option ([], []) $ xtParserNonOptional
  arrow
  t'' <- tParser
  v <- option (VarConst "") varParser
  return
    (VdStmt
       Vd {nameVd = name, varsVd = zip y' k', typesVd = zip b' t', toVd = t''})

-- | operation parser
odParser :: Parser ElementStmt
odParser = do
  rword "function"
  name <- identifier
  colon
  (y', k') <- option ([], []) $ brackets ykParser
  (b', t') <- option ([], []) xtParser
  arrow
  t'' <- tParser
  v <- option (VarConst "") varParser
  return
    (OdStmt
       Od {nameOd = name, varsOd = zip y' k', typesOd = zip b' t', toOd = t''})

-- | predicate parser
pdParser, pd1, pd2 :: Parser ElementStmt
pdParser = try pd2 <|> pd1

pd1 = do
  rword "predicate"
  name <- identifier
  colon
  (y', k') <- option ([], []) $ brackets ykParser
  (b', t') <- option ([], []) xtParser
  return
    (PdStmt
       (Pd1Const
          (Pd1 {namePd1 = name, varsPd1 = zip y' k', typesPd1 = zip b' t'})))

pd2 = do
  rword "predicate"
  name <- identifier
  colon
  (b', prop') <- xPropParser
  return (PdStmt (Pd2Const (Pd2 {namePd2 = name, propsPd2 = zip b' prop'})))

snParser :: Parser ElementStmt
snParser = do
  rword "notation"
  quote
  toSn' <- manyTill anySingle quote
  tilde
  quote
  fromSn' <- manyTill anySingle quote
  return (SnStmt (Sn {fromSn = fromSn', toSn = toSn'}))

-------------------------- Element Semantic Checker -------------------------------
-- | 'check' is the top-level semantic checking function. It takes a Element
-- program as the input, checks the validity of the program acoording to the
-- typechecking rules, and outputs a collection of information.
check :: ElementProg -> VarEnv
check p =
  let env = foldl checkElementStmt initE p
  in if null (errors env)
       then env
       else error $
            "Element type checking failed with the following problems: \n" ++
            (ppShow $ errors env) ++ ppShow env
  where
    initE =
      VarEnv
      { typeConstructors = M.empty
      , valConstructors = M.empty
      , operators = M.empty
      , predicates = M.empty
      , typeVarMap = M.empty
      , typeValConstructor = M.empty
      , varMap = M.empty
      , subTypes = []
      , stmtNotations = []
      , typeCtorNames = []
      , preludes = []
      , declaredNames = []
      , errors = ""
      }

checkElementStmt :: VarEnv -> ElementStmt -> VarEnv
checkElementStmt e (CdStmt c) =
  let kinds = seconds (inputCd c)
      env1 = foldl checkK e kinds
      tc = TypeConstructor {nametc = nameCd c, kindstc = kinds}
      ef = addName (nameCd c) env1
  in ef {typeConstructors = M.insert (nameCd c) tc $ typeConstructors ef}
checkElementStmt e (SubtypeDeclStmt s) =
  let env1 = checkDeclaredType e (subType s)
      env2 = checkDeclaredType env1 (superType s)
      env3 = env2 {subTypes = (subType s, superType s) : subTypes env2}
  in env3
checkElementStmt e (PreludeDeclStmt (VarConst pvar) ptype) =
  let env = checkT e ptype
  in env {preludes = ((VarConst pvar), ptype) : preludes env}
checkElementStmt e (VdStmt v) =
  let kinds = seconds (varsVd v)
      env1 = foldl checkK e kinds
      localEnv = foldl updateEnv env1 (varsVd v)
      args = seconds (typesVd v)
      res = toVd v
      env2 = foldl checkT localEnv args
      env3 = checkT env2 res
      vc =
        ValConstructor
        { namevc = nameVd v
        , ylsvc = firsts (varsVd v)
        , kindsvc = seconds (varsVd v)
        , nsvc = firsts (typesVd v)
        , tlsvc = seconds (typesVd v)
        , tvc = toVd v
        }
      e1 = addName (nameVd v) env3
      ef = addValConstructor vc e1
  in if env2 == e || env2 /= e && env3 == e || env3 /= e && e1 == e || e1 /= e
       then ef {valConstructors = M.insert (nameVd v) vc $ valConstructors ef}
       else error "Error!" -- Does not suppose to reach here
checkElementStmt e (OdStmt v) =
  let kinds = seconds (varsOd v)
      env1 = foldl checkK e kinds
      localEnv = foldl updateEnv env1 (varsOd v)
      args = seconds (typesOd v)
      res = toOd v
      env2 = foldl checkT localEnv args
      env3 = checkT env2 res
      op =
        Operator
        { nameop = nameOd v
        , ylsop = firsts (varsOd v)
        , kindsop = seconds (varsOd v)
        , tlsop = seconds (typesOd v)
        , top = toOd v
        }
      ef = addName (nameOd v) env3
  in if env2 == e || env2 /= e && env3 == e || env3 /= e
       then ef {operators = M.insert (nameOd v) op $ operators ef}
       else error "Error!" -- Does not suppose to reach here
checkElementStmt e (PdStmt (Pd1Const v)) =
  let kinds = seconds (varsPd1 v)
      env1 = foldl checkK e kinds
      localEnv = foldl updateEnv env1 (varsPd1 v)
      args = seconds (typesPd1 v)
      env2 = foldl checkT localEnv args
      pd1 =
        Pred1 $
        Prd1
        { namepred1 = namePd1 v
        , ylspred1 = firsts (varsPd1 v)
        , kindspred1 = seconds (varsPd1 v)
        , tlspred1 = seconds (typesPd1 v)
        }
      ef = addName (namePd1 v) e
  in if env2 == e || env2 /= e
       then ef {predicates = M.insert (namePd1 v) pd1 $ predicates ef}
       else error "Error!" -- Does not suppose to reach here
checkElementStmt e (PdStmt (Pd2Const v)) =
  let pd = Pred2 $ Prd2 {namepred2 = namePd2 v, plspred2 = seconds (propsPd2 v)}
      ef = addName (namePd2 v) e
  in ef {predicates = M.insert (namePd2 v) pd $ predicates ef}
checkElementStmt e (SnStmt s) =
  let (from, to, patterns, entities) = T.translatePatterns (fromSn s, toSn s) e
      newSnr =
        StmtNotationRule
        { fromSnr = from
        , toSnr = to
        , patternsSnr = patterns
        , entitiesSnr = entities
        }
  in e {stmtNotations = newSnr : stmtNotations e}

computeSubTypes :: VarEnv -> VarEnv
computeSubTypes e =
  let env1 = e {subTypes = transitiveClosure (subTypes e)}
  in if isClosureNotCyclic (subTypes env1)
       then env1
       else env1 {errors = errors env1 ++ "Cyclic Subtyping Relation! \n"}

-- | 'parseElement' runs the actual parser function: 'elementParser', taking in a
--   program String, parses it and semantically checks it. It outputs the
--   environement as returned from the element typechecker
parseElement :: String -> String -> Either CompilerError VarEnv
parseElement elementFile elementIn =
  case runParser elementParser elementFile elementIn of
    Left err -> Left $ ElementParse $ (errorBundlePretty err)
    Right prog ->
      let env = check prog
          env1 = computeSubTypes env
      in Right env1

----------------------------- Test Driver --------------------------------------
-- | For testing: first uncomment the module definition to make this module the
-- Main module. Usage: ghc Element.hs; ./Element <element-file> <output-file>
main :: IO ()
main = do
  [elementFile, outputFile] <- getArgs
  elementIn <- readFile elementFile
  case parse elementParser elementFile elementIn of
    Left err -> putStr (errorBundlePretty err)
    Right xs -> do
      writeFile outputFile (show xs)
      let o = check xs
      print o
  putStrLn "Parsing Done!"
  return ()
