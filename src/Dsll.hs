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
import Env
--import Text.PrettyPrint
--import Text.PrettyPrint.HughesPJClass hiding (colon, comma, parens, braces)
import qualified Data.Map.Strict as M
import qualified Text.Megaparsec.Char.Lexer as L
--------------------------------------- DSLL AST ---------------------------------------

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
    show (DSLLProg cd vd od pd) = "types = " ++ cdString ++ "\n \n" ++ "vars = " ++
        vdString ++ "\n \n" ++ "operations = " ++ odString ++ "\n \n" ++ "predicates = " ++ pdString ++ "\n \n"
        where cdString = show cd
              vdString = show vd
              odString = show od
              pdString = show pd

-- --------------------------------------- DSLL Parser -------------------------------------


-- | 'DSLLParser' is the top-level parser function. The parser contains a list of functions
--    that parse small parts of the language. When parsing a source program, these functions are invoked
--    in a top-down manner.
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


-- | type constructor parser
cdParser, cd1, cd2 :: Parser Cd
cdParser = try cd1 <|> cd2
cd1= do
    rword "tconstructor"
    name <- identifier
    (y, k) <- parens ykParser
    colon
    t' <- typeParser
    return Cd {nameCd = name, inputCd = zip y k, outputCd =  t'}
cd2 = do
    rword "tconstructor"
    name <- identifier
    colon
    t' <- typeParser
    return Cd {nameCd = name, inputCd =  [], outputCd =  t'}


varWithType = do
    v <- varParser
    colon
    t <- tParser
    return (v, t)
yWithKind = do
    y <- yParser
    colon
    k <- kParser
    return (y, k)

-- | parser for the (y,k) list, refactored out to prevent duplication
ykParser :: Parser ([Y],[K])
ykParser = unzip <$> (yWithKind `sepBy1` comma)

-- | parser for the (b,t) list, refactored out to prevent duplication
xtParser :: Parser ([Var],[T])
xtParser = unzip <$> (varWithType `sepBy1` comma)

-- | var constructor parser
vdParser :: Parser Vd
vdParser = do
  rword "vconstructor"
  name <- identifier
  (y',k') <- option ([], []) ykParser
  (b',t') <- option ([], []) xtParser
  colon
  t'' <- tParser
  return Vd{nameVd = name, varsVd = (zip y' k'), typesVd  =  (zip b' t'), toVd = t''}

  -- | operation parser
odParser :: Parser Od
odParser = do
  rword "operator"
  name <- identifier
  (y',k') <- option ([], []) $ brackets ykParser
  (b',t') <- option ([], []) $ parens   xtParser
  colon
  t'' <- tParser
  return Od{nameOd = name, varsOd = (zip y' k'), typesOd  =  (zip b' t'), toOd = t''}

-- | predicate parser
pdParser, pd1, pd2 :: Parser Pd
pdParser = try pd1 <|> pd2
pd1 = do
  rword "predicate"
  name <- identifier
  (y',k') <- option ([], []) $ brackets ykParser
  (b',t') <- option ([], []) $ parens   xtParser
  colon
  p' <- propParser
  return (Pd1Const (Pd1{namePd1 = name, varsPd1 = (zip y' k'), typesPd1  =  (zip b' t'), toPd1 = p'}))
pd2 = do
  rword "predicate"
  name <- identifier
  lparen
  b' <- brackets (varParser `sepBy1` comma)
  colon
  prop' <- brackets (propParser `sepBy1` comma)
  rparen
  colon
  p' <- propParser
  return (Pd2Const (Pd2{namePd2 = name, propsPd2 = (zip b' prop'), toPd2 = p'}))


-- --------------------------------------- DSLL Semantic Checker ---------------------------


-- | 'check' is the top-level semantic checking function. It takes a DSLL
-- program as the input, checks the validity of the program acoording to the typechecking rules, and outputs
-- a collection of information.
check :: DSLLProg -> VarEnv
check p =  let env1  = foldl checkTypeConstructors initE (cd p)
               env2  = foldl checkVarConstructors env1 (vd p)
               env3  = foldl checkOperations env2 (od p)
               env4  = foldl checkPredicates env3 (pd p)
           in if (null (errors env4)) then env4 else error("DSLL type checking failed with the following problems: \n" ++ (errors env4))
           where initE = VarEnv {typeContructors = M.empty, varConstructors = M.empty,
            operations = M.empty, predicates = M.empty, typeVarMap = M.empty, varMap = M.empty, names = [], errors = ""}


checkTypeConstructors :: VarEnv -> Cd -> VarEnv
checkTypeConstructors e c = let kls = (seconds (inputCd c))
                                env1 = foldl checkK e kls
                                tc = TypeConstructor {nametc = nameCd c, klstc = (seconds (inputCd c))
                                                       , typtc = outputCd c}
                                ef = addName (nameCd c) env1
                             in ef {typeContructors = M.insert (nameCd c) tc $ typeContructors ef}


checkVarConstructors :: VarEnv -> Vd -> VarEnv
checkVarConstructors e v = let kls = (seconds (varsVd v))
                               env1 = foldl checkK e kls
                               localEnv = foldl updateEnv env1 (varsVd v)
                               args = (seconds (typesVd v))
                               res = (toVd v)
                               env2 = foldl checkT localEnv args
                               temp = checkT localEnv res
                               vc = VarConstructor {namevc = nameVd v,  ylsvc = (firsts (varsVd v))
                                         , klsvc = (seconds (varsVd v)) , tlsvc = (seconds (typesVd v)), tvc = (toVd v)}
                               ef = addName (nameVd v) e
                            in if ((env2 == e || env2 /= e) && (temp == e || temp /= e)) then ef{varConstructors = M.insert (nameVd v) vc $ varConstructors ef}
                               else error ("Error!") -- Does not suppose to reach here


checkOperations :: VarEnv -> Od -> VarEnv
checkOperations e v = let kls = (seconds (varsOd v))
                          env1 = foldl checkK e kls
                          localEnv = foldl updateEnv env1 (varsOd v)
                          args = (seconds (typesOd v))
                          res = (toOd v)
                          env2 = foldl checkT localEnv args
                          temp = checkT localEnv res
                          op = Operation {nameop = nameOd v,  ylsop = (firsts (varsOd v))
                                    , klsop = (seconds (varsOd v)) , tlsop = (seconds (typesOd v)), top = (toOd v)}
                          ef = addName (nameOd v) e
                        in if ((env2 == e || env2 /= e) && (temp == e || temp /= e)) then ef{operations = M.insert (nameOd v) op $ operations ef}
                          else error ("Error!")  -- Does not suppose to reach here


checkPredicates :: VarEnv -> Pd -> VarEnv
checkPredicates e (Pd1Const v) = let kls = (seconds (varsPd1 v))
                                     env1 = foldl checkK e kls
                                     localEnv = foldl updateEnv env1 (varsPd1 v)
                                     args = (seconds (typesPd1 v))
                                     env2 = foldl checkT localEnv args
                                     pd1 = Pred1 Predicate1 {namepred1 = namePd1 v,  ylspred1 = (firsts (varsPd1 v))
                                         , klspred1 = (seconds (varsPd1 v)) , tlspred1 = (seconds (typesPd1 v)), ppred1 = (toPd1 v)}
                                     ef = addName (namePd1 v) e
                                  in if ((env2 == e || env2 /= e)) then ef{predicates = M.insert (namePd1 v) pd1 $ predicates ef}
                                   else error ("Error!")  -- Does not suppose to reach here

checkPredicates e (Pd2Const v) = let pd = Pred2 Predicate2 {namepred2 = namePd2 v, plspred2 = (seconds (propsPd2 v)) , ppred2 = (toPd2 v)}
                                     ef = addName (namePd2 v) e
                        in ef {predicates = M.insert (namePd2 v) pd $ predicates ef}


-- | 'parseDsll' runs the actual parser function: 'dsllParser', taking in a program String, parses it and
-- | semantically checks it. It outputs the environement as returned from the dsll typechecker
parseDsll :: String -> String -> IO VarEnv
parseDsll dsllFile dsllIn = case runParser dsllParser dsllFile dsllIn of
     Left err -> error (parseErrorPretty err)
     Right xs -> do
         let env = check xs
         return (env)


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
