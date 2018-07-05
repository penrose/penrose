-- | "DSLL" contains the grammers and parser for the
--    DSLL language
--    Author: Dor Ma'ayan, May 2018

{-# OPTIONS_HADDOCK prune #-}
module Dsll where
--module Main (main) where -- for debugging purposes

import Utils
import System.Process
import Control.Monad (void)
import Data.Void
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
import Env
--import Text.PrettyPrint
--import Text.PrettyPrint.HughesPJClass hiding (colon, comma, parens, braces)
import qualified Data.Map.Strict as M
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------- DSLL AST ---------------------------------------

type DsllProg = [DsllStmt]

data DsllStmt = CdStmt Cd
             | VdStmt Vd
             | SubtypeDeclStmt SubtypeDecl
             | OdStmt Od
             | PdStmt Pd
             deriving (Show, Eq, Typeable)

-- | tconstructor
data Cd = Cd { nameCd   :: String,
               inputCd  :: [(Y, K)],
               outputCd :: Type }
          deriving (Eq, Typeable)

instance Show Cd where
    show (Cd nameCd inputCd outputCd) = "(TCon, " ++ nString ++ ", ValOfType " ++ iString ++ ", Output " ++ oString ++")"
        where nString = show nameCd
              iString = show inputCd
              oString = show outputCd

-- | vconstructor
data Vd = Vd { nameVd  :: String,
               varsVd  :: [(Y, K)],
               typesVd :: [(Var, T)],
               toVd    :: T }
          deriving (Eq, Typeable)

instance Show Vd where
    show (Vd nameVd varsVd typesVd toVd) =
     "(VCon, " ++ aString ++ ", forvars " ++ bString ++ ", fortypes " ++ cString ++ ", outputT " ++ dString ++ ")"
        where aString = show nameVd
              bString = show varsVd
              cString = show typesVd
              dString = show toVd

-- | Subtype declarations
data SubtypeDecl = SubtypeDecl { subType :: T,
                                 superType :: T }
                   deriving (Eq, Typeable)

instance Show SubtypeDecl where
   show (SubtypeDecl subType superType) = aString ++ "Subtype of" ++ bString
      where aString = show subType
            bString = show superType

-- | predicates
data Od = Od { nameOd  :: String,
               varsOd  :: [(Y, K)],
               typesOd :: [(Var, T)],
               toOd    :: T}
          deriving (Eq, Typeable)

instance Show Od where
    show (Od nameOd varsOd typesOd toOd) =
     "(Op, " ++ aString ++ ", forvars " ++ bString ++ ", fortypes " ++ cString ++ ", outputT " ++ dString ++ ")"
        where aString = show nameOd
              bString = show varsOd
              cString = show typesOd
              dString = show toOd

-- | predicates
data Pd = Pd1Const Pd1
        | Pd2Const Pd2
        deriving (Show, Eq, Typeable)

data Pd1 = Pd1 { namePd1  :: String,
                 varsPd1  :: [(Y, K)],
                 typesPd1 :: [(Var, T)],
                 toPd1    :: Prop}
           deriving (Eq, Typeable)

instance Show Pd1 where
    show (Pd1 namePd1 varsPd1 typesPd1 toPd1) =
     "(Pred, " ++ aString ++ ", forvars " ++ bString ++ ", fortypes " ++ cString ++ ", outputT " ++ dString ++ ")"
        where aString = show namePd1
              bString = show varsPd1
              cString = show typesPd1
              dString = show toPd1

data Pd2 = Pd2 { namePd2 :: String,
                 propsPd2 :: [(Var, Prop)],
                 toPd2 :: Prop}
           deriving (Eq, Typeable)

instance Show Pd2 where
    show (Pd2 namePd2 propsPd2 toPd2) =
     "(Pred, " ++ aString ++ ", forProps " ++ bString ++ ", outputT " ++ cString ++ ")"
        where aString = show namePd2
              bString = show propsPd2
              cString = show toPd2

----------------------------------------- DSLL Parser -------------------------------------

-- | 'DSLLParser' is the top-level parser function. The parser contains a list of functions
--    that parse small parts of the language. When parsing a source program, these functions are invoked
--    in a top-down manner.
dsllParser :: Parser [DsllStmt]
dsllParser = between scn eof dsllProgParser -- Parse all the statemnts between the spaces to the end of the input file

-- |'dsllProg' parses the entire actual DSLL program which is a collection of constructors followed by a collection of
--   operations followed by a collection of predicates
dsllProgParser :: Parser [DsllStmt]
dsllProgParser = dsllStmt `sepEndBy` newline'

dsllStmt :: Parser DsllStmt
dsllStmt = try cdParser <|> try vdParser <|> try odParser <|> try subtypeDeclParser <|> try pdParser

-- | type constructor parser
cdParser, cd1, cd2 :: Parser DsllStmt
cdParser = try cd1 <|> cd2
cd1 = do
    rword "tconstructor"
    name <- identifier
    (y, k) <- parens ykParser
    colon
    t' <- typeParser
    pos <- getPosition
    return (CdStmt Cd { nameCd = name, inputCd = zip y k, outputCd = t' })
cd2 = do
    rword "tconstructor"
    name <- identifier
    colon
    t' <- typeParser
    return (CdStmt Cd { nameCd = name, inputCd = [], outputCd = t' })

-- | sub type declarations parser
subtypeDeclParser :: Parser DsllStmt
subtypeDeclParser = do
  subtype <- tParser
  rword "<:"
  supertype <- tParser
  return $ SubtypeDeclStmt $ SubtypeDecl { subType = subtype, superType = supertype}

-- | parser for the (y,k) list
ykParser :: Parser ([Y], [K])
ykParser = unzip <$> (yWithKind `sepBy1` comma)
    where yWithKind = (,) <$> (yParser <* colon) <*> kParser

-- | parser for the (b,t) list
xtParser :: Parser ([Var], [T])
xtParser = unzip <$> (varWithType `sepBy1` comma)
    where varWithType = (,) <$> (varParser <* colon) <*> tParser

-- | parser for the (x, Prop) list
xPropParser :: Parser ([Var], [Prop])
xPropParser = unzip <$> (vpPair `sepBy1` comma)
    where vpPair = (,) <$> (varParser <* colon) <*> propParser

-- | var constructor parser
vdParser :: Parser DsllStmt
vdParser = do
  rword "vconstructor"
  name <- identifier
  (y', k') <- option ([], []) $ brackets ykParser
  (b', t') <- option ([], []) $ parens   xtParser
  colon
  t'' <- tParser
  return (VdStmt Vd { nameVd = name, varsVd = zip y' k', typesVd = zip b' t', toVd = t'' })

-- | operation parser
odParser :: Parser DsllStmt
odParser = do
  rword "operator"
  name <- identifier
  (y', k') <- option ([], []) $ brackets ykParser
  (b', t') <- option ([], []) $ parens   xtParser
  colon
  t'' <- tParser
  return (OdStmt Od { nameOd = name, varsOd = zip y' k', typesOd = zip b' t', toOd = t'' })

-- | predicate parser
pdParser, pd1, pd2 :: Parser DsllStmt
pdParser = try pd1 <|> pd2
pd1 = do
  rword "predicate"
  name <- identifier
  (y', k') <- option ([], []) $ brackets ykParser
  (b', t') <- option ([], []) $ parens   xtParser
  colon
  p' <- propParser
  return (PdStmt (Pd1Const (Pd1 { namePd1 = name, varsPd1 = zip y' k', typesPd1 = zip b' t', toPd1 = p' })))
pd2 = do
  rword "predicate"
  name <- identifier
  (b', prop') <- parens xPropParser
  colon
  p' <- propParser
  return (PdStmt (Pd2Const (Pd2 { namePd2 = name, propsPd2 = zip b' prop', toPd2 = p' })))

--------------------------------------- DSLL Semantic Checker ---------------------------

-- | 'check' is the top-level semantic checking function. It takes a DSLL
-- program as the input, checks the validity of the program acoording to the typechecking rules, and outputs
-- a collection of information.

check :: DsllProg -> VarEnv
check p = let env = foldl checkDsllStmt initE p
          in if null (errors env)
             then env
             else error $ "Dsll type checking failed with the following problems: \n" ++ errors env
             where initE = VarEnv { typeConstructors = M.empty, valConstructors = M.empty,
                                    operators = M.empty, predicates = M.empty, typeVarMap = M.empty,
                                    varMap = M.empty, subTypes = [], typeCtorNames = [], declaredNames = [],
                                    errors = ""}

checkDsllStmt :: VarEnv -> DsllStmt -> VarEnv
checkDsllStmt e (CdStmt c) = let kinds  = seconds (inputCd c)
                                 env1 = foldl checkK e kinds
                                 tc   = TypeConstructor { nametc = nameCd c, kindstc = kinds, typtc = outputCd c }
                                 ef   = addName (nameCd c) env1
                             in ef { typeConstructors = M.insert (nameCd c) tc $ typeConstructors ef }

checkDsllStmt e (SubtypeDeclStmt s) = let env1 = checkDeclaredType e (subType s)
                                          env2 = checkDeclaredType env1 (superType s)
                                          env3 = env2 { subTypes = (subType s,superType s) : subTypes env2 }
                                       in env3

checkDsllStmt e (VdStmt v) = let kinds = seconds (varsVd v)
                                 env1 = foldl checkK e kinds
                                 localEnv = foldl updateEnv env1 (varsVd v)
                                 args = seconds (typesVd v)
                                 res = toVd v
                                 env2 = foldl checkT localEnv args
                                 temp = checkT localEnv res
                                 vc = ValConstructor { namevc = nameVd v, ylsvc = firsts (varsVd v),
                                                       kindsvc = seconds (varsVd v), tlsvc = seconds (typesVd v),
                                                       tvc = toVd v }
                                 ef = addName (nameVd v) e
                              in if env2 == e || env2 /= e && temp == e || temp /= e
                                  then ef { valConstructors = M.insert (nameVd v) vc $ valConstructors ef }
                                  else error "Error!" -- Does not suppose to reach here

checkDsllStmt e (OdStmt v) = let kinds = seconds (varsOd v)
                                 env1 = foldl checkK e kinds
                                 localEnv = foldl updateEnv env1 (varsOd v)
                                 args = seconds (typesOd v)
                                 res = toOd v
                                 env2 = foldl checkT localEnv args
                                 temp = checkT env2 res
                                 op = Operator { nameop = nameOd v, ylsop = firsts (varsOd v),
                                                 kindsop = seconds (varsOd v), tlsop = seconds (typesOd v), top = toOd v }
                                 ef = addName (nameOd v) e
                               in if env2 == e || env2 /= e && temp == e || temp /= e
                                   then ef { operators = M.insert (nameOd v) op $ operators ef }
                                   else error "Error!"  -- Does not suppose to reach here


checkDsllStmt e (PdStmt (Pd1Const v)) = let kinds = seconds (varsPd1 v)
                                            env1 = foldl checkK e kinds
                                            localEnv = foldl updateEnv env1 (varsPd1 v)
                                            args = seconds (typesPd1 v)
                                            env2 = foldl checkT localEnv args
                                            pd1 = Pred1 $ Prd1 { namepred1 = namePd1 v,
                                                                 ylspred1  = firsts (varsPd1 v),
                                                                 kindspred1  = seconds (varsPd1 v),
                                                                 tlspred1  = seconds (typesPd1 v),
                                                                 ppred1    = toPd1 v }
                                            ef = addName (namePd1 v) e
                                        in if env2 == e || env2 /= e
                                            then ef { predicates = M.insert (namePd1 v) pd1 $ predicates ef }
                                            else error "Error!"  -- Does not suppose to reach here

checkDsllStmt e (PdStmt (Pd2Const v)) = let pd = Pred2 $ Prd2 { namepred2 = namePd2 v, plspred2 = seconds (propsPd2 v),
                                                                ppred2 = toPd2 v }
                                            ef = addName (namePd2 v) e
                                         in ef { predicates = M.insert (namePd2 v) pd $ predicates ef }

computeSubTypes :: VarEnv -> VarEnv
computeSubTypes e = let env1 = e { subTypes = transitiveClosure (subTypes e)}
                    in if isClosureNotCyclic (subTypes env1) then
                      env1
                    else env1 { errors = errors env1 ++ "Cyclic Subtyping Relation! \n"}


-- | 'parseDsll' runs the actual parser function: 'dsllParser', taking in a program String, parses it and
-- | semantically checks it. It outputs the environement as returned from the dsll typechecker
parseDsll :: String -> String -> IO VarEnv
parseDsll dsllFile dsllIn =
          case runParser dsllParser dsllFile dsllIn of
          Left err -> error (parseErrorPretty err)
          Right prog -> do
              putStrLn "DSLL AST: \n"
              print prog
              divLine
              let env = check prog
                  env1 = computeSubTypes env
              divLine
              putStrLn "DSLL Env: \n"
              print env1
              return env1

-- --------------------------------------- Test Driver -------------------------------------

-- | For testing: first uncomment the module definition to make this module the
-- Main module. Usage: ghc Dsll.hs; ./Dsll <dsll-file> <output-file>

main :: IO ()
main = do
  [dsllFile, outputFile] <- getArgs
  dsllIn <- readFile dsllFile
  case parse dsllParser dsllFile dsllIn of
    Left err -> putStr (parseErrorPretty err)
    Right xs -> do
      writeFile outputFile (show xs)
      let o = check xs
      print o
  putStrLn "Parsing Done!"
  return ()
