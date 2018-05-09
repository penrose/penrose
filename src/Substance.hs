-- | "Substance" contains the grammar, parser, and semantic checker for
--   the Substance language. It also contains translators to Alloy and
--   the driver for it.

{-# OPTIONS_HADDOCK prune #-}
module Substance where
-- module Main (main) where -- for debugging purposes
-- TODO split this up + do selective export

import Utils
import System.Process
import Control.Monad (void)
import System.IO -- read/write to file
import System.Environment
import Control.Arrow ((>>>))
import System.Random
import Debug.Trace
import Data.List
import Data.Maybe (fromMaybe)
import Data.Typeable
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
-- import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass hiding (colon, comma, parens, braces)
import qualified Data.Map.Strict as M
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------
-- Substance AST

-- | A Substance program is a list of statements
type SubProg = [SubStmt]
type SubObjDiv = ([SubDecl], [SubConstr])

-- | A statement can either be a declaration of some mathematical object, or a
--   definition (used by Alloy)
data SubStmt
    = Decl SubType String -- type, id
    | DeclList SubType [String] -- Allowing comma separated declarations
    | ConstrDecl SubType [String] -- type, arguments
    | SetInit SubType String [String]
    | MapInit SubType String String String -- id, from, to
    | SurjectionInit SubType String String String -- id, from, to
    | BijectionInit SubType String String String -- id, from, to
    | InjectionInit SubType String String String -- id, from, to
    | FuncVal String String String -- function name, x, y
    | Def String [(SubType, String)] FOLExpr -- id, definition string
    | DefApp String [String] -- function id, args
    | NoStmt
    deriving (Show, Eq, Typeable)

-- | A First-order logic expression models elements used in definitions of functions.
data FOLExpr
    = QuantAssign Quant Binders FOLExpr
    | BinaryOp Op FOLExpr FOLExpr
    | FuncAccess String String
    | TermID String
    deriving (Show, Eq, Typeable)
data Op = AND | OR | NOT | IMPLIES | EQUAL | NEQ deriving (Show, Eq, Typeable)
data Quant = FORALL | EXISTS deriving (Show, Eq, Typeable)
type Binders = [([String], String)]

-- | 'SubType' contains all types in the Substance language
data SubType
    = SetT
    | PointT
    | MapT
    | SurjectionT
    | BijectionT
    | InjectionT
    | IntersectT
    | NoIntersectT
    | NoSubsetT
    | SubsetT
    | PointInT
    | PointNotInT
    | ValueT
    | AllT  -- specifically for global selection in Style
    | DerivedType SubType -- TODO: inheritance of types? Openset
    deriving (Show, Eq, Typeable)

-- | Both declarations and constaints in Substance are regarded as objects, which is possible for Style to select later.
data SubObj = LD SubDecl | LC SubConstr deriving (Show, Eq, Typeable)

-- | Declaration of Substance objects
data SubDecl
    = Set String
    | Map String String String
    | Surjection String String String
    | Injection String String String
    | Bijection String String String
    | Value String String String
    | Point String
    deriving (Show, Eq, Typeable)

-- | Declaration of Substance constaints
data SubConstr
    = Intersect String String
    | NoIntersect String String
    | Subset String String
    | NoSubset String String
    | PointIn String String
    | PointNotIn String String
    deriving (Show, Eq, Typeable)

-- objTypes = [ SetT, PointT, MapT ]
-- constrTypes = [ IntersectT, NoIntersectT, NoSubsetT, SubsetT, PointInT,
--                 PointNotInT, FuncValT ]

--------------------------------------------------------------------------------
-- Parser for Substance

-- | 'substanceParser' is the top-level parser function. The parser contains a list of functions that parse small parts of the language. When parsing a source program, these functions are invoked in a top-down manner.
substanceParser :: Parser [SubStmt]
substanceParser = between scn eof subProg

-- | 'subProg' parses the entire Substance program, which is a collection of statments
subProg :: Parser [SubStmt]
subProg =  subStmt `sepEndBy` newline'

subStmt :: Parser SubStmt
subStmt = try subDef <|> try subDecl <|> defApp

-- TODO: think about why the `try` is needed here?
-- NOTE: I used `try`s because some of the sub-functions will actally consume tokens even when it fails to parse the whole thing. As a result, the next sub-function will actually starts from where the previous function left off, making it impossible to parse.
subDecl, varDecl, setInit, funcDecl, surjectionDecl, bijectionDecl, injectionDecl :: Parser SubStmt
subDecl = try setInit <|> try funcVal <|> try surjectionDecl <|> try bijectionDecl <|> try injectionDecl <|> try varDecl <|> try constrDecl <|> try funcDecl
constrDecl = do
    -- a <- identifier
    typ <- subConstrType
    -- b <- identifier
    args <- some identifier
    return (ConstrDecl typ args)
varDecl = do
    typ <- subObjType
    ids <- identifier `sepBy1` comma
    if length ids == 1
        then return (Decl typ (head ids))
        else return (DeclList typ ids)
setInit = do
    rword "Set"
    i <- identifier
    void (symbol "=")
    ids <- braces $ identifier `sepBy1` comma
    return (SetInit SetT i ids)
funcDecl = do
    i <- identifier
    colon
    a <- identifier
    arrow
    b <- identifier
    return (MapInit MapT i a b)
surjectionDecl = do
    rword "Surjection"
    i <- identifier
    a <- identifier
    b <- identifier
    return (SurjectionInit SurjectionT i a b)
injectionDecl = do
    rword "Injection"
    i <- identifier
    a <- identifier
    b <- identifier
    return (InjectionInit InjectionT i a b)
bijectionDecl = do
    rword "Bijection"
    i <- identifier
    a <- identifier
    b <- identifier
    return (BijectionInit BijectionT i a b)
funcVal = do
    f <- some alphaNumChar
    a <- parens identifier
    void (symbol "=")
    b <- identifier
    return (FuncVal f a b)

subDef :: Parser SubStmt
subDef = do
    rword "Definition"
    i    <- identifier
    args <- parens $ bindings `sepBy1` comma
    colon >> newline'
    t     <- folExpr
    -- str <- some printChar
    -- return (Def i str)
    return (Def i args t)
    where bindings = (,) <$> subtype <*> identifier

-- | Applying a definition to a Substance object. For example,
--
-- > f: A -> B
-- > Set A
-- > Set B
-- > Definition Surjection(Map f, Set X, Set Y):
--     > forall y : Y | exists x : X | f(x) = y
-- > Surjection(f, A, B)
--
-- Here the definition @Surjection@ is applied to a 'Map' with id @f@
defApp :: Parser SubStmt
defApp = do
    n <- identifier
    args <- parens $ identifier `sepBy1` comma
    return (DefApp n args)

folExpr :: Parser FOLExpr
folExpr = makeExprParser folTerm folOps

-- | Expressions in FOL can be combined together by logical operators. Here we are using an expression builder, provided by Megaparsec library function 'makeExprParser'.
folOps :: [[Operator Parser FOLExpr]]
folOps =
    [ [ InfixL (BinaryOp EQUAL   <$ symbol "=")
      , InfixL (BinaryOp NEQ     <$ symbol "!=") ]
    , [ InfixL (BinaryOp AND     <$ symbol "/\\") ]
    , [ InfixR (BinaryOp IMPLIES <$ symbol "implies" )]
    , [ InfixL (BinaryOp OR      <$ symbol "\\/") ]
    ]
folTerm = try quantAssign <|> try funcAccess <|> TermID <$> try identifier

quantAssign, funcAccess :: Parser FOLExpr
quantAssign = do
    q <- quant
    bs <- binders
    void (symbol "|")
    e <- folExpr
    return (QuantAssign q bs e)
    -- return (QuantAssign q bs (TermID "ja"))
funcAccess = do
    t1 <- identifier
    t2 <- parens identifier
    return (FuncAccess t1 t2)

binders :: Parser Binders
binders = binder `sepBy1` comma
    where binder = (,) <$> (identifier `sepBy1` comma) <* colon <*> identifier
quant = (symbol "forall" >> return FORALL) <|>
        (symbol "exists" >> return EXISTS)

subtype :: Parser SubType
subtype = subObjType <|> subConstrType
subObjType =
        (rword "Set"         >> return SetT)               <|>
        -- (rword "OpenSet"     >> return (DerivedType SetT)) <|>
        (rword "Point"       >> return PointT)             <|>
        (rword "Value"       >> return ValueT)
subConstrType =
        (rword "Subset"      >> return SubsetT)      <|>
        (rword "Map"         >> return MapT)         <|>
        (rword "Surjection"  >> return SurjectionT)  <|>
        (rword "Bijection"  >> return BijectionT)    <|>
        (rword "Injection"  >> return InjectionT)    <|>
        (rword "NoSubset"    >> return NoSubsetT)    <|>
        (rword "Intersect"   >> return IntersectT)   <|>
        (rword "NoIntersect" >> return NoIntersectT) <|>
        (rword "In"          >> return PointInT)     <|>
        (rword "NotIn"       >> return PointNotInT)

--------------------------------------------------------------------------------
-- Semantic checker and desugaring

-- | Environment for the semantic checker. As the 'check' function executes, it
-- accumulate information such as symbol tables in the environment.
data SubEnv = SubEnv {
    subObjs :: [SubObj],  -- declared Substance objects(including constraints, which is, again, viewed also as objects)
    subDefs :: M.Map String ([(SubType, String)], FOLExpr), -- all definitions
    subApps :: [(FOLExpr, M.Map String String, [String])], -- Def, varmap, ids to be solved by alloy
    subSymbols :: M.Map String SubType, -- symbol table
    subArgs :: M.Map String [String]
    -- subAppliedDefs :: [FOLExpr]
} deriving (Show, Eq, Typeable)

-- | 'check' is the top-level semantic checking function. It takes a Substance
-- program as the input, checks the validity of the program, and outputs
-- a collection of information.
-- The check is done in two passes. First check all the declarations of
-- stand-alone objects like `Set`, and then check for all other things
check :: SubProg -> SubEnv
check p = let env1 = foldl checkDecls initE p
              env2 = foldl checkReferencess env1 p
              in
            env2 { subObjs = reverse $ subObjs env2 }
          where initE = SubEnv { subObjs = [], subDefs = M.empty,  subSymbols = M.empty, subApps = [], subArgs = M.empty }

applyDef (n, m) d = case M.lookup n d of
    Nothing -> error "applyDef: definition not found!"
    Just (_, e) -> e

-- | 'checkDecls' checks the validity of declarations of objects.
checkDecls :: SubEnv -> SubStmt -> SubEnv
checkDecls e (Decl t s)  = e { subObjs = toObj t [s] : subObjs e, subSymbols = checkAndInsert s t $ subSymbols e }
checkDecls e (DeclList t ss) = e { subObjs = objs, subSymbols = syms }
    where objs = subObjs e ++ map (toObj t . toList) ss
          syms = foldl (\p x -> checkAndInsert x t p) (subSymbols e) ss
checkDecls e (MapInit t f a b) =
    e { subSymbols = checkAndInsert f t $ subSymbols e, subArgs = a1 }
    where a1 = M.insert f [a, b] $ subArgs e
checkDecls e (SurjectionInit t f a b) =
    e { subSymbols = checkAndInsert f t $ subSymbols e, subArgs = a1 }
    where a1 = M.insert f [a, b] $ subArgs e
checkDecls e (BijectionInit t f a b) =
    e { subSymbols = checkAndInsert f t $ subSymbols e, subArgs = a1 }
    where a1 = M.insert f [a, b] $ subArgs e
checkDecls e (InjectionInit t f a b) =
    e { subSymbols = checkAndInsert f t $ subSymbols e, subArgs = a1 }
    where a1 = M.insert f [a, b] $ subArgs e
checkDecls e (Def n a f) = e { subDefs = M.insert n (a, f) $ subDefs e }
checkDecls e (SetInit t i ps) =
    let pts =  map (toObj PointT . toList) ps
        set = toObj t [i]
        ptConstrs = map (toConstr PointInT . (\p -> [p, i])) ps
        m1  = foldl (\p x -> checkAndInsert x PointT p) (subSymbols e) ps
    in
    e { subObjs = pts ++ [set] ++ ptConstrs ++ subObjs e, subSymbols = checkAndInsert i t m1 }
checkDecls e _ = e -- Ignore all other statements

-- 'toObj' translates [Type] + [Identiers] to concrete Substance objects, to be selected by the Style program
toObj :: SubType -> [String] -> SubObj
toObj SetT [i]         = LD $ Set i
toObj PointT [i]       = LD $ Point i
toObj MapT [i, a, b]   = LD $ Map i a b
toObj SurjectionT [i, a, b]   = LD $ Surjection i a b
toObj BijectionT [i, a, b]   = LD $ Bijection i a b
toObj InjectionT [i, a, b]   = LD $ Injection i a b
toObj ValueT [f, a, b] = LD $ Value f a b
toObj t os             = error ("toObj: incorrect arguments to " ++ show t ++ " "++ show os)

-- 'checkReferencess' checks any statement that refers to other objects. For example,
-- > Subset A B
-- refers to identifiers @A@ and @B@. The function will perform a lookup in the symbol table, make sure the objects exist and are of desired types -- in this case, 'Set' -- and throws an error otherwise.
checkReferencess :: SubEnv -> SubStmt -> SubEnv
checkReferencess e (ConstrDecl t ss)  = e { subObjs = newConstrs : subObjs e }
    where newConstrs = toConstr t $ map (checkNameAndTyp $ subSymbols e) $ zip ss ts
          ts = case t of
              PointInT    -> [PointT, SetT]
              PointNotInT -> [PointT, SetT]
              _           -> [SetT, SetT]
checkReferencess e (FuncVal f a b)  = e { subObjs = val : subObjs e }
    where args = map (checkNameAndTyp $ subSymbols e) $ zip [f, a, b] [MapT, PointT, PointT]
          val  = toObj ValueT args

checkReferencess e (MapInit t f a b) = e { subObjs = toObj t args : subObjs e }
    where args = map (checkNameAndTyp $ subSymbols e) $ zip [f, a, b] [MapT, SetT, SetT]

checkReferencess e (SurjectionInit t f a b) = e { subObjs = toObj t args : subObjs e }
    where args = map (checkNameAndTyp $ subSymbols e) $ zip [f, a, b] [SurjectionT, SetT, SetT]

checkReferencess e (InjectionInit t f a b) = e { subObjs = toObj t args : subObjs e }
    where args = map (checkNameAndTyp $ subSymbols e) $ zip [f, a, b] [InjectionT, SetT, SetT]

checkReferencess e (BijectionInit t f a b) = e { subObjs = toObj t args : subObjs e }
    where args = map (checkNameAndTyp $ subSymbols e) $ zip [f, a, b] [BijectionT, SetT, SetT]

checkReferencess e (DefApp n args) = e { subApps = (def, apps, funcToSolve ++ setsToSolve) : subApps e }
    where (sigs, def)  = fromMaybe (error ("Definition " ++ n ++ " does not exist.")) (M.lookup n (subDefs e))
          argsWithTyps = zip args $ map fst sigs
          args' = map (checkNameAndTyp $ subSymbols e) argsWithTyps
          apps  = M.fromList $ zip (map snd sigs) args
          funcToSolve = map fst $ filter (\(n, t) -> t == MapT) argsWithTyps
          setsToSolve =  concatMap (\x -> fromMaybe (error ("Function " ++ x ++ "does not exist.")) (M.lookup x $ subArgs e)) funcToSolve
checkReferencess e _ = e -- Ignore all other statements

-- | Similar to 'toObj'
toConstr :: SubType -> [String] -> SubObj
toConstr NoIntersectT [a, b] = LC $ NoIntersect a b
toConstr IntersectT [a, b] = LC $ Intersect a b
toConstr PointInT [a, b] = LC $ PointIn a b
toConstr PointNotInT [a, b] = LC $ PointNotIn a b
toConstr SubsetT [a, b] = LC $ Subset a b
toConstr NoSubsetT [a, b] = LC $ NoSubset a b
toConstr t os = error ("toConstr: incorrect arguments to " ++ show t ++ " "++ show os)

-- helper function that checks the symbol table before inserting, making sure there is no duplicated declarations
checkAndInsert s t m = case M.lookup s m of
    Nothing -> M.insert s t m
    _ -> error ("Duplicated symbol: " ++ s)
-- helper function that checks the existence and type of an object
checkNameAndTyp m (s, t) = case M.lookup s m of
    Nothing -> error ("Undefined symbol: " ++ s)
    Just t' -> if t == t' then s
            else error ("Type of " ++ s ++ " is incorrect. Expecting " ++ show t ++ " , but have " ++ show t')

-- | `subSeparate` aplits a list of Substance objects into declared objects and constaints on these objects
subSeparate :: [SubObj] -> SubObjDiv
subSeparate = foldr separate ([], [])
            where separate line (decls, constrs) =
                           case line of
                           (LD x) -> (x : decls, constrs)
                           (LC x) -> (decls, x : constrs)


-- | 'parseSubstance' runs the actual parser function: 'substanceParser', taking in a program String, parses it, semantically checks it, and eventually invoke Alloy if needed. It outputs a collection of Substance objects at the end.
parseSubstance :: String -> String -> IO [SubObj]
parseSubstance subFile subIn = case runParser substanceParser subFile subIn of
     Left err -> error (parseErrorPretty err)
     Right xs -> do
         mapM_ print xs
         divLine
         let c = check xs
         if null $ subApps c
             then return (subObjs c)
             else runAlloy c

-- | "runAlloy" is the driver function for the Alloy Analyzer. It would take the
--   current Substance AST, translate to Alloy, and retrieve Substance objects
--   generated by Alloy. As a side effect, a temp file of the translated
--   Substance will get generated.
runAlloy :: SubEnv -> IO [SubObj]
runAlloy c = do
    let al = toAlloy c
    -- FIXME: here we only assume each application of def will yield one
    -- id to be solved by Alloy, might not be the case in the future
    let toSolve = rmdups $ concatMap trd $ subApps c
    let setsToSolve = concatMap snd $ M.toList $ subArgs c
    print setsToSolve
    mapM_ print al
    divLine
    mapM_ (putStrLn . prettyShow) al
    let pretty_al = concatMap ((++ "\n") . prettyShow)  al
    writeFile  (alloyDir ++ alloyTempFile) pretty_al
    let args = [ "runAlloy.sh", alloyTempFile, show alloyNumSamples] ++ toSolve
    print args
    res <- readProcess "bash" args ""
    putStr res
    case runParser fromAlloy "" res of
        Left err -> error (parseErrorPretty err)
        Right objs -> do
            print objs
            return (subObjs $ c { subObjs =  subObjs c ++ objs })
-- rmdups :: (Ord a) => [a] -> [a]
    where rmdups = map head . group . sort



--------------------------------------------------------------------------------
-- Simplified Alloy AST and translators
-- See reference for the Alloy modeling language here:
--  http://alloy.mit.edu/alloy/documentation/book-chapters/alloy-language-reference.pdf
-- TODO: separate to another module?

-- | Each Alloy program is a collection of paragraphs
type AlProg = [AlPara]
-- | A "paragraph" in Alloy, in our case, can be either a command or declaration
data AlPara
    = SigDecl String [AlDecl] -- a signature is just a set declaration
    | OneSigDecl String String -- a special "one sig" in Alloy means a single point
    | PredDecl String
    | FactDecl [AlExpr]
    | RunCmd String (Maybe Int) -- run command that tells Alloy to generate instances with optionally an upperbound on the number of instances
    deriving (Show, Eq, Typeable)
-- NOTE: this is okay if we model everything as the same type of relations
data AlDecl = AlDecl String String
    deriving (Show, Eq, Typeable)
data AlExpr
    = AlFuncVal String String String
    | AlProp FOLExpr (M.Map String String)
    deriving (Show, Eq, Typeable)
data AlBinaryOp = AlDot | AlEq deriving (Show, Eq, Typeable)

-- | Substance to Alloy translation environment:
data AlEnv = AlEnv {
    alFacts :: [AlExpr],
    alSigs  :: M.Map String AlPara
} deriving (Show, Eq, Typeable)

-- | 'toAlloy' translates a semantically checked Substance program to an Alloy program in AST form, which can be then pretty-printed.
toAlloy :: SubEnv -> AlProg
toAlloy e =  M.elems (alSigs resEnv) ++ rest
    where initEnv = AlEnv { alFacts = [], alSigs = M.empty }
          objEnv  = foldl objToAlloy initEnv $ subObjs e
          resEnv  = foldl defToAlloy objEnv $ subApps e
          rest = [FactDecl (alFacts resEnv), showPred, runFor "show" 5]
        --   rest = [FactDecl (alFacts resEnv), showPred, runNoLimit "show"]

-- | default components in an Alloy program, for showing instances
showPred = PredDecl "show"
runNoLimit s = RunCmd s Nothing
runFor s i = RunCmd s (Just i)

-- | 'objToAlloy' translates an object in Substance to its counterpart in Alloy land.
objToAlloy :: AlEnv -> SubObj -> AlEnv
objToAlloy e (LD (Set s)) = e { alSigs = insertSig s (SigDecl s []) $ alSigs e}
objToAlloy e (LC (PointIn p s)) = e { alSigs = M.insert p (OneSigDecl p s) $ alSigs e }
objToAlloy e (LD (Value f x y)) = e { alFacts = fac : alFacts e }
    where  fac = AlFuncVal f x y
        -- fac = AlBinOp AlEq (AlBinOp AlDot x f) y
objToAlloy e (LD (Map f x y)) = e { alSigs = newSigs }
    where  l' = AlDecl f y : l
           newSigs = M.insert x (SigDecl n l') m
           (SigDecl n l, m) = case M.lookup x $ alSigs e of
                Nothing -> let sig = SigDecl x [] in (sig, M.insert x sig $ alSigs e)
                Just s -> (s, alSigs e)


objToAlloy e _ = e -- Ignoring all other Substance objects

-- To make sure the ordering doesn't matter. For example, if we have a Function
-- declared before the sets, the translator will generate the signatures
insertSig n s e = case M.lookup n e of
    Nothing -> M.insert n s e
    _ -> e

defToAlloy :: AlEnv -> (FOLExpr, M.Map String String, [String]) -> AlEnv
defToAlloy e (f, m, _) =  e { alFacts = AlProp f m : alFacts e }

-- | pretty-printing class for Alloy AST. We are using a pretty-printer combinator for this task. See <https://hackage.haskell.org/package/pretty-1.1.3.5/docs/Text-PrettyPrint-HughesPJ.html> for details.
instance Pretty AlPara where
    pPrint (SigDecl n ds) = vcat (header : map (nest 4 . pPrint) ds) $$ rbrace
        where header = text "sig" <+> text n <+> lbrace
    pPrint (OneSigDecl s e) = text "one sig" <+> text s <+> text "extends" <+> text e <> lbrace <+> rbrace
    pPrint (PredDecl s) = text "pred" <+> text (s ++ "()") <+> lbrace <+> rbrace
    pPrint (FactDecl es) = vcat (header : map (nest 2 . pPrint) es) $$ rbrace
        where header =  text "fact" <+> lbrace
    pPrint (RunCmd s i) = text "run" <+> text s <+> num
        where num = case i of
                        Nothing -> text ""
                        Just i' -> text "for" <+> text (show i')
instance Pretty AlDecl where
    pPrint (AlDecl f s) = text f <+> text ":" <+> text s
instance Pretty AlExpr where
    pPrint (AlFuncVal f x y) = text x <> text "." <> text f <+> text "=" <+> text y
    pPrint (AlProp s varMap) = pPrintExpr s varMap

pPrintExpr :: FOLExpr -> M.Map String String -> Doc
pPrintExpr s varMap = case s of
    QuantAssign q b e -> let (varMap', bs) = foldl bind (varMap, []) b
                             pBs = map pBind bs in
                         pPrint q <+> hcat (punctuate (text ", ") pBs) <+> text "|" <+> pPrintExpr e varMap'
    BinaryOp op e1 e2 -> pPrintExpr e1 varMap <+> pPrint op <+> pPrintExpr e2 varMap
    FuncAccess f x -> text (r x) <> text "." <> text (r f)
    TermID i -> text (r i)
    where bind (m, res) (a, s) = case M.lookup s m of
                             Nothing -> error ("Undefined variable: " ++ s)
                             Just s' -> (foldl (\m x -> M.insert x x m) varMap a, (a, s') : res)
          pBind (a, b) = hcat (punctuate (text ",") $ map text a) <+> text ":" <+> text b
          resolve m x = fromMaybe (error ("Undefined variable: " ++ x)) (M.lookup x m)
          r = resolve varMap

instance Pretty Quant where
    pPrint FORALL = text "all"
    pPrint EXISTS = text "some"

instance Pretty Op where
    pPrint IMPLIES = text "implies"
    pPrint EQUAL   = text "="
    pPrint NEQ     = text "!="
    pPrint AND     = text "and"
    pPrint OR      = text "or"

-- | "AlloyOut" represents the output of the Alloy evaluator
fromAlloy :: Parser [SubObj]
fromAlloy = concat <$> endBy line newline
    where line = try funcLine <|> setLine
          funcLine = do
              f <- identifier
              colon
              braces (mapping f `sepBy1` comma)
          setLine = do
              s <- identifier
              colon
              concat <$> braces (point s `sepBy1` comma)
          -- alloyId = someTill anyChar (symbol "$") <*> some numberChar
          alloyId = do
              a <- someTill anyChar (symbol "$")
              b <- some numberChar
              return (a ++ b)
          mapping f = do
              a <- alloyId
              arrow
              b <- alloyId
              return (toObj ValueT [f, a, b])
          point s = do
              a <- alloyId
              return [toObj PointT [a], toConstr PointInT [a, s]]


--------------------------------------------------------------------------------
-- Test driver: First uncomment the module definition to make this module the -- Main module. Usage: ghc Substance; ./Substance <substance-file>

parseFromFile p file = runParser p file <$> readFile file

main :: IO ()
main = do
    args <- getArgs
    let subFile = head args
    subIn <- readFile subFile
    parsed <- parseSubstance subFile subIn
    mapM_ print parsed
    return ()

alloyDir = "./"
alloyTempFile = "__temp__.als"
alloyNumSamples = 1
