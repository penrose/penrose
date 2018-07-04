-- | The "Style" module contains the compiler for the Style language,
-- and functions to traverse the Style AST, which are used by "Runtime"

{-# OPTIONS_HADDOCK prune #-}
module NewStyle where
-- module Main (main) where -- for debugging purposes

import qualified Shapes as S
import Utils
import Control.Monad (void, foldM)
import Data.Function (on)
import Data.Either (partitionEithers)
import Data.Either.Extra (fromLeft)
import Data.Maybe (fromMaybe, catMaybes, isNothing, maybeToList)
import Data.List (nubBy, nub, intercalate)
import Data.Tuple (swap)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import Text.Megaparsec.Perm
import System.Environment
import Debug.Trace
import qualified Substance as C
import Functions (objFuncDict, constrFuncDict, ObjFnOn, Weight, ConstrFnOn, ConstrFnInfo, ObjFnInfo)
import Computation
import qualified Data.Map.Strict as M
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Dynamic
import Data.Typeable
import Env

--------------------------------------------------------------------------------
-- Style AST

-------------------- Style Program grammar

-- | A variable in Style
newtype StyVar  = StyVar' String
    deriving (Show, Eq, Ord, Typeable)

-- | A header of a block is either a selector or a namespace declaration
data Header = Select Selector | Namespace StyVar
    deriving (Show, Eq, Typeable)

-- | A Style block contains a list of statements
type Block = [Stmt]

-- | A Style program is a collection of (header, block) pairs
type StyProg = [(Header, Block)]

-------------------- Style selector grammar
-- TODO: write a NOTE about the namespace situation between Substance and Style

data STypeVar = STypeVar' {
    typeVarNameS :: String,
    typeVarPosS  :: SourcePos
} deriving (Eq, Typeable)
instance Show STypeVar where
    show = show . typeVarNameS

-- | A type in Style can be TODO (Alias because of T in `Env`)
data StyT = STTypeVar STypeVar
          | STCtor STypeCtor
      deriving (Show, Eq, Typeable)

data STypeCtor = STypeCtor {
    nameConsS :: String,
    argConsS  :: [SArg],
    posConsS  :: SourcePos
} deriving (Eq, Typeable)
instance Show STypeCtor where
    show t = show (nameConsS t) ++ show (argConsS t)

data SArg
    = SAVar BindingForm
    | SAT StyT
    deriving (Show, Eq, Typeable)

data SelExpr
    = SEBind BindingForm
    | SEAppFunc String [SelExpr]
    | SEAppValCons String [SelExpr]
    deriving (Show, Eq, Typeable)

data PredArg
    = PE SelExpr -- TODO: this should only be allowed to be a Var in Sub (and maybe in Sty?)
    | PP Predicate
    deriving (Show, Eq, Typeable)

data Predicate = Predicate {
    predicateName :: String,
    predicateArgs :: [PredArg],
    predicatePos  :: SourcePos
} deriving (Eq, Typeable)

instance Show Predicate where
    show t = show (predicateName t) ++ show (predicateArgs t)

-- | A binding form can either be a newly declared Style id or an existing
-- Substance id, which is denoted by the special @``@ characters
data BindingForm = BSubVar Var | BStyVar StyVar
    deriving (Show, Eq, Ord, Typeable)

data DeclPattern = PatternDecl' StyT BindingForm
    deriving (Show, Eq, Typeable)

data RelationPattern = RelBind BindingForm SelExpr | RelPred Predicate
    deriving (Show, Eq, Typeable)

-- | A selector is TODO
data Selector = Selector {
    selHead  :: [DeclPattern],
    selWith  :: [DeclPattern],
    selWhere :: [RelationPattern],
    selNamespace :: Maybe String
} deriving (Show, Eq, Typeable)

-------------------- Style block grammar

type Field = String

-- | A path consist of a Substance or Style id, a shape name, and (optionally)
-- a property name
data Path
    = FieldPath BindingForm Field                 -- example: x.val
    | PropertyPath BindingForm Field Property     -- example: x.shape.center
    -- NOTE: Style writer must use backticks in the block to indicate Substance variables
    deriving (Show, Eq, Typeable)

-- | A statement in the Style language
data Stmt
    = Assign Path Expr
    | Override Path Expr
    | Delete Path
    deriving (Show, Eq, Typeable)

-- | A field declaration in a Style constructor binds an expression to a
-- string
data PropertyDecl = PropertyDecl String Expr
    deriving (Show, Eq, Typeable)

data AnnoFloat = Fix Float | Vary
    deriving (Show, Eq, Typeable)

-- | An expression in the Style language
-- TODO: wrap custom types around the raw strings/typedef them for better error checking??
data Expr
    = IntLit Integer
    | AFloat AnnoFloat
    | StringLit String
    | EPath Path
    | CompApp String [Expr]
    | ObjFn String [Expr]
    | ConstrFn String [Expr]
    | AvoidFn String [Expr]  -- TODO: to be implemented
    | BinOp BinaryOp Expr Expr
    | UOp UnaryOp Expr
    | List [Expr]
    | ListAccess Path Integer
    | Ctor String [PropertyDecl]  -- TODO: use the existing styObj parser in Sty.hs!
    | Layering LExpr
    deriving (Show, Eq, Typeable)

data LExpr
    = LId BindingForm
    | LPath Path
    | LayeringOp LayerOp LExpr LExpr
    deriving (Show, Eq, Typeable)

data LayerOp  = Less | LessEq | Eq | Seq
    deriving (Show, Eq, Typeable)

data UnaryOp  = UPlus | UMinus
    deriving (Show, Eq, Typeable)

data BinaryOp = BPlus | BMinus | Multiply | Divide | Exp
    deriving (Show, Eq, Typeable)

--------------------------------------------------------------------------------
-- Style Parser

-- | 'parseStyle' runs the actual parser function: 'styleParser', taking in a program String and parse it into an AST.
parseStyle :: String -> String -> IO StyProg
parseStyle styFile styIn =
    case runParser styleParser styFile styIn of
    Left err -> error (parseErrorPretty err)
    Right styProg -> return styProg

-- | 'styleParser' is the top-level function that parses a Style proram
styleParser :: Parser StyProg
styleParser = between scn eof styProg

-- | `styProg` parses a Style program, consisting of a collection of one or
-- more blocks
styProg :: Parser StyProg
styProg = some headerBlock
    where headerBlock = (,) <$> header <*> braces block <* scn

header :: Parser Header
header = tryChoice [Select <$> selector, Namespace <$> styVar]

-------------------- Selector parsers
-- COMBAK: short-hand for decls with the same type
-- COMBAK: change commas to semicolons

-- TODO: clear up the `scn` calls for all parsers and establish the convention of calling scn AFTER each parser
selector :: Parser Selector
selector = do
    hd       <- fmap concat $ declPattern `sepBy1` semi <* scn
    (wi, wh) <- withAndWhere
    ns       <- optional $ namespace <* scn
    return Selector { selHead = hd,  selWith = wi, selWhere = wh,
                      selNamespace = ns}
    where wth = fmap concat $ rword "with" *> declPattern `sepEndBy1` semi <* scn
          whr = rword "where" *> relationPattern `sepEndBy1` semi <* scn
          namespace = rword "as" >> identifier
          withAndWhere = makePermParser $ (,) <$?> ([], wth) <|?> ([], whr)

styVar :: Parser StyVar
styVar = StyVar' <$> identifier

declPattern :: Parser [DeclPattern]
declPattern = do
    t  <- styType
    ns <- bindingForm `sepBy1` comma
    return $ map (PatternDecl' t) ns

styType :: Parser StyT
styType = STTypeVar <$> typeVar <|> STCtor <$> typeConstructor

typeVar :: Parser STypeVar
typeVar = do
    aps -- COMBAK: get rid of this later once it's consistent with DSLL/Substance
    t   <- identifier
    pos <- getPosition
    return STypeVar' { typeVarNameS = t, typeVarPosS = pos}

typeConstructor :: Parser STypeCtor
typeConstructor = do
    n    <- identifier
    args <- option [] $ parens (styArgument `sepBy1` comma)
    pos  <- getPosition
    return STypeCtor { nameConsS = n, argConsS = args, posConsS = pos }

-- COMBAK: styType will probably not work because of recursion. Test this explicitly
styArgument :: Parser SArg
styArgument = SAVar <$> bindingForm <|> SAT <$> styType

relationPattern :: Parser RelationPattern
relationPattern = try pre <|> bind
    where bind = RelBind <$> bindingForm <*> (def >> selectorExpr)
          pre  = RelPred <$> predicate

predicate :: Parser Predicate
predicate = do
    n    <- identifier
    args <- parens $ predicateArgument `sepBy1` comma
    pos  <- getPosition
    return Predicate { predicateName = n, predicateArgs = args,
                       predicatePos = pos }

predicateArgument :: Parser PredArg
predicateArgument = PE <$> selectorExpr <|> PP <$> predicate

selectorExpr :: Parser SelExpr
selectorExpr =
    tryChoice [
        -- COMBAK: right recursion, empty parens
        -- TODO: document that value constructors should start w/ a capital letter and functions w/ lowercase
        SEAppValCons <$> upperId <*> parens (selectorExpr `sepBy1` comma),
        SEAppFunc    <$> lowerId <*> parens (selectorExpr `sepBy1` comma),
        SEBind       <$> bindingForm
    ]

bindingForm :: Parser BindingForm
bindingForm = BSubVar <$> backticks varParser <|> BStyVar <$> styVar

-------------------- Block parsers

block :: Parser [Stmt]
block = stmt `sepEndBy` newline'

stmt :: Parser Stmt
stmt = tryChoice [assign, override, delete]

assign, override, delete :: Parser Stmt
assign   = Assign   <$> path <*> (eq >> expr)
override = Override <$> (rword "override" >> path) <*> (eq >> expr)
delete   = Delete   <$> (rword "delete"   >> path)

expr :: Parser Expr
expr = tryChoice [
           constructor,
           objFn,
           constrFn,
           compFn,
           Layering <$> brackets layeringExpr,
           list,
           stringLit,
           arithmeticExpr
       ]

-- COMBAK: change NewStyle to Style
arithmeticExpr :: Parser Expr
arithmeticExpr = makeExprParser aTerm NewStyle.aOperators

aTerm :: Parser Expr
aTerm = parens arithmeticExpr
    <|> try (AFloat <$> annotatedFloat)
    <|> EPath  <$> path
    <|> IntLit <$> integer

aOperators :: [[Text.Megaparsec.Expr.Operator Parser Expr]]
aOperators =
    [   -- Highest precedence
        [
            Prefix (UOp UMinus <$ symbol "-"),
            Prefix (UOp UPlus  <$ symbol "+")
        ],
        [ InfixL (BinOp Exp <$ symbol "^") ],
        [
            InfixL (BinOp Multiply <$ symbol "*"),
            InfixL (BinOp Divide   <$ symbol "/")
        ],
        [
            InfixL (BinOp BPlus  <$ symbol "+"),
            InfixL (BinOp BMinus <$ symbol "-")
        ]
        -- Lowest precedence
    ]

layeringExpr :: Parser LExpr
layeringExpr = makeExprParser lTerm NewStyle.lOperators

lTerm :: Parser LExpr
lTerm =
    tryChoice [
        parens layeringExpr,
        LPath <$> path,
        LId   <$> bindingForm
    ]

lOperators :: [[Text.Megaparsec.Expr.Operator Parser LExpr]]
lOperators =
    [   -- Highest precedence
        [ InfixL (LayeringOp Seq <$ symbol ",") ],
        [
            InfixL (LayeringOp Less   <$ symbol "<"),
            InfixL (LayeringOp LessEq <$ symbol "<="),
            InfixL (LayeringOp Eq     <$ symbol "==")
        ]
        -- Lowest precedence
    ]

path :: Parser Path
path = try (PropertyPath <$> bindingForm <*> dotId <*> dotId) <|>
       FieldPath <$> bindingForm <*> dotId
       where dotId = dot >> identifier

compFn, objFn, constrFn :: Parser Expr
compFn   = CompApp <$> identifier <*> exprsInParens
objFn    = ObjFn <$> (rword "encourage" >> identifier) <*> exprsInParens
constrFn = ConstrFn <$> (rword "ensure" >> identifier) <*> exprsInParens
exprsInParens = parens $ expr `sepBy` comma

list :: Parser Expr
list = List <$> brackets (expr `sepBy1` comma)

constructor :: Parser Expr
constructor = do
    typ    <- identifier
    fields <- braces (propertyDecl `sepEndBy` newline')
    return $ Ctor typ fields

propertyDecl :: Parser PropertyDecl
propertyDecl = PropertyDecl <$> identifier <*> (eq >> expr)

stringLit :: Parser Expr
stringLit = StringLit <$> (char '"' >> manyTill L.charLiteral (char '"'))

annotatedFloat :: Parser AnnoFloat
annotatedFloat = (rword "OPTIMIZED" *> pure Vary) <|> Fix <$> float

------------------------------------------------------------------------
-- Style static semantics for selectors

------------ Type defs

-- g ::= B => |T
-- Assumes nullary type constructors (i.e. Style type = Substance type)
data SelEnv = SelEnv { sTypeVarMap :: M.Map BindingForm StyT, -- B : |T
                       sErrors :: [String] }
              deriving (Show, Eq, Typeable)

type Error = String

------------ Helper functions on envs

initSelEnv :: SelEnv
initSelEnv = SelEnv { sTypeVarMap = M.empty, sErrors = [] }

-- g, x : |T
addMapping :: BindingForm -> StyT -> SelEnv -> SelEnv
addMapping k v m = m { sTypeVarMap = M.insert k v (sTypeVarMap m) }

addErr :: Error -> SelEnv -> SelEnv
addErr err selEnv = selEnv { sErrors = err : sErrors selEnv }

addErrs :: [Error] -> SelEnv -> SelEnv
addErrs errs selEnv = selEnv { sErrors = sErrors selEnv ++ errs }

-- TODO: don't merge the varmaps! just put g as the varMap (otherwise there will be extraneous bindings for the relational statements)
-- Judgment 1. G || g |-> ...
mergeEnv :: VarEnv -> SelEnv -> VarEnv
mergeEnv varEnv selEnv = foldl mergeMapping varEnv (M.assocs $ sTypeVarMap selEnv)
         where mergeMapping :: VarEnv -> (BindingForm, StyT) -> VarEnv
               -- G || (x : |T) |-> G
               mergeMapping varEnv (BSubVar var, styType) = varEnv
               -- G || (y : |T) |-> G[y : T] (shadowing any existing Sub vars)
               mergeMapping varEnv (BStyVar (StyVar' var), styType) = 
                            varEnv { varMap = M.insert (VarConst var) (toSubType styType) (varMap varEnv) }

----------- Converting Style values to Env/Substance values for two reasons:
-- 1. so they can be put into Gamma, 2. so the normal Substance judgments can be reused (e.g. T : type)

-- | All Style variables "y" are converted to Substance vars "`x`"
toSubVar :: BindingForm -> Var
toSubVar (BSubVar subv)           = subv
toSubVar (BStyVar (StyVar' styv)) = VarConst styv

toSubTArg :: SArg -> Arg
toSubTArg (SAVar bVar) = AVar $ toSubVar bVar
toSubTArg (SAT styT)   = AT   $ toSubType styT

toSubType :: StyT -> T
toSubType (STTypeVar stvar) = TTypeVar $ TypeVar { typeVarName = typeVarNameS stvar, 
                                                   typeVarPos = typeVarPosS stvar }
toSubType (STCtor stcons) = TConstr $ TypeCtorApp { nameCons = nameConsS stcons, 
                                                      argCons = map toSubTArg $ argConsS stcons,
                                                      constructorInvokerPos = posConsS stcons }

toSubExpr :: SelExpr -> C.Expr
toSubExpr (SEBind bvar) = C.VarE $ toSubVar bvar
toSubExpr (SEAppFunc f exprs) = C.ApplyFunc $ C.Func { C.nameFunc = f, C.argFunc = map toSubExpr exprs }
toSubExpr (SEAppValCons v exprs) = C.ApplyValCons $ C.Func { C.nameFunc = v, C.argFunc = map toSubExpr exprs }

toSubPredArg :: PredArg -> C.PredArg
toSubPredArg (PE selExpr) = C.PE $ toSubExpr selExpr
toSubPredArg (PP pred)    = C.PP $ toSubPred pred

toSubPred :: Predicate -> C.Predicate
toSubPred stypred = C.Predicate { C.predicateName = C.PredicateConst $ predicateName stypred, 
                                  C.predicateArgs = map toSubPredArg $ predicateArgs stypred, 
                                  C.predicatePos = predicatePos stypred }

----------- Selector static semantics

-- For `B := E`, make sure `B : T` and `E : T`
compareTypes :: RelationPattern -> BindingForm -> SelExpr -> Maybe T -> Maybe T -> Maybe Error
compareTypes stmt var expr vtype etype = 
   case (vtype, etype) of 
   (Nothing, Nothing) ->
     Just $ "In Style statement '" ++ show stmt ++ "', the var and expr" ++ " are both of invalid type." 
   (Just vtype', Nothing) ->
     Just $ "In Style statement '" ++ show stmt ++ "', the expr '" ++ show expr ++
            " should have type '" ++ show vtype' ++ "' but does not have a type."
   (Nothing, Just etype') ->
     Just $ "In Style statement '" ++ show stmt ++ "', the var '" ++ show var ++
            " should have type '" ++ show etype' ++ "' but does not have a type."
   (Just vtype', Just etype') ->
     if typesEq (trM2 ("VTYPE: " ++ show vtype') vtype') (trM2 ("ETYPE: " ++ show etype') etype') then Nothing
     else Just $ "In Style statement '" ++ show stmt ++ "', the var and expr" ++
                 " should have the same type, but have types '" ++ show vtype' ++
                 "' and '" ++ show etype' ++ "'."

-- Judgment 5. G |- [|S_r] ok
checkRelPatterns :: VarEnv -> [RelationPattern] -> [Error]
checkRelPatterns varEnv rels = concat $ map (checkRelPattern varEnv) rels
    -- Judgment 4. G |- |S_r ok
    where checkRelPattern :: VarEnv -> RelationPattern -> [Error]
          checkRelPattern varEnv rel = 
              case rel of
              -- rule Bind-Context
              RelBind bVar sExpr -> 
                      -- TODO: use checkSubStmt here (and in paper)?
                      -- TODO: make sure the ill-typed bind selectors fail here (after Sub statics is fixed)
                      -- G |- B : T1
                      let (error1, vtype) = C.checkVarE varEnv $ 
                                            (trM2 ("B: " ++ show (toSubVar bVar)) $ toSubVar bVar) in
                      -- G |- E : T2
                      let (error2, etype) = C.checkExpression varEnv $ 
                                            (trM2 ("E: " ++ show (toSubExpr sExpr)) $ toSubExpr sExpr) in
                      -- T1 = T2
                      let err3 = compareTypes rel bVar sExpr vtype etype in
                      case trM2 ("ERR3: " ++ show err3) err3 of 
                      Nothing -> [error1, error2]
                      Just e3 -> [error1, error2, e3]
              -- rule Pred-Context
              RelPred pred       -> 
                      -- G |- Q : Prop
                      let varEnv' = C.checkPredicate varEnv $ toSubPred pred in
                      [errors varEnv']

-- Judgment 6. G; g |- [|S_o] ~> g'
checkDeclPatterns :: VarEnv -> SelEnv -> [DeclPattern] -> SelEnv
checkDeclPatterns varEnv selEnv decls = foldl (checkDeclPattern varEnv) selEnv decls
    -- Judgment 3. G; g |- |S_o ok ~> g'
    where checkDeclPattern :: VarEnv -> SelEnv -> DeclPattern -> SelEnv
          checkDeclPattern varEnv selEnv stmt@(PatternDecl' styType bVar) = 
             -- G |- |T : type
             let errT = errors $ checkT varEnv (toSubType styType) in 
             let selEnv' = addErr errT selEnv in
             case bVar of 
             -- rule Decl-Sty-Context
             bsv@(BStyVar (StyVar' styVar)) ->
                     -- NOTE: this does not aggregate *all* possible errors. May just return first error.
                     -- y \not\in dom(g)
                     if M.member bsv (sTypeVarMap selEnv')
                     then let err = "Style pattern statement " ++ show stmt ++ 
                                    " declares Style variable '" ++ styVar ++ "' twice"
                          in addErr err selEnv'
                     else if M.member (BSubVar (VarConst styVar)) (sTypeVarMap selEnv')
                     then let err = "Style pattern statement " ++ show stmt ++ 
                                    " declares Style variable '" ++ styVar ++ "'" ++
                                    " in the same selector as a Substance variable of the same name"
                          in addErr err selEnv'
                     else addMapping bsv styType selEnv'
             -- rule Decl-Sub-Context
             bsv@(BSubVar subVar@(VarConst sVar)) -> 
                     -- x \not\in dom(g)
                     if M.member bsv (sTypeVarMap selEnv')
                     then let err = "Style pattern statement " ++ show stmt ++ 
                                    " declares Substance variable '" ++ sVar ++ "' twice"
                          in addErr err selEnv'
                     else if M.member (BStyVar (StyVar' sVar)) (sTypeVarMap selEnv')
                     then let err = "Style pattern statement " ++ show stmt ++ 
                                    " declares Substance variable '" ++ sVar  ++ "'" ++
                                    " in the same selector as a Style variable of the same name"
                          in addErr err selEnv'
                     else 
                         -- G(x) = T
                         let subType = M.lookup subVar $ varMap varEnv in
                         case subType of
                         Nothing -> let err = "Substance variable '" ++ show subVar ++ 
                                              "' does not exist in environment. \n" {- ++ show varEnv -} in
                                    addErr err selEnv'
                         Just subType' -> 
                             -- check "T = |T", assuming type constructors are nullary
                             let declType = toSubType styType in
                             if subType' == declType 
                             then addMapping bsv styType selEnv'
                             else let err = "Mismatched types between Substance and Style vars.\n" ++
                                             "Sub var '" ++ show subVar ++ "' has type '" ++ show subType' ++
                                             "in Substance but has type '" ++ show declType ++ "' in Style."
                                  in addErr err selEnv'

-- Judgment 7. G |- Sel ok ~> g
checkSel :: VarEnv -> Selector -> SelEnv
checkSel varEnv sel = 
         -- Check head statements
         let selEnv_afterHead = checkDeclPatterns varEnv initSelEnv (selHead sel) in
         -- Check `with` statements
         let selEnv_decls = checkDeclPatterns varEnv selEnv_afterHead (selWith sel) in
         -- Check relational statements
         let rel_errs = checkRelPatterns (mergeEnv varEnv selEnv_decls) (selWhere sel) in
         selEnv_decls { sErrors = filter (/= "") $ reverse (sErrors selEnv_decls) ++ rel_errs }

-- Returns a sel env for each selector in the Style program, in the same order
-- TODO: add these judgments to the paper
checkSels :: VarEnv -> StyProg -> [SelEnv]
checkSels varEnv prog = map (checkPair varEnv) prog
          where checkPair :: VarEnv -> (Header, Block) -> SelEnv
                checkPair varEnv ((Select sel), _) = checkSel varEnv sel
                checkPair varEnv ((Namespace name), _) = initSelEnv 
                -- TODO: for now, namespace has no local context 

-- TODO: write test cases (including failing ones) after Substance errors are no longer @ runtime
-- TODO: get line/col numbers for errors

----------- Selector dynamic semantics (matching)

----- Type declarations

-- A substitution θ has form [y → x], binding Sty vars to Sub vars (currently not expressions).
type Subst = M.Map StyVar Var

-- TODO: add beta everywhere after merge

----- Debug info

debugM1, debugM2, debugM3 :: Bool
debugM1 = False
debugM2 = False
debugM3 = False

mkTr :: Bool -> String -> a -> a
mkTr flag str res = if flag then trace str res else res

-- matching
trM1 = mkTr debugM1
-- statics
trM2 = mkTr debugM2
-- translation
trM3 = mkTr debugM3

----- Substitution helper functions

-- (+) operator
combine :: Ord k => M.Map k a -> M.Map k a -> M.Map k a
combine s1 s2 = M.union s1 s2
-- TODO check for duplicate keys (and vals)

-- (x) operator 
merge :: Ord k => [M.Map k a] -> [M.Map k a] -> [M.Map k a]
merge s1 [] = s1
merge [] s2 = s2
merge s1 s2 = [ combine s1i s2j | s1i <- s1, s2j <- s2 ] -- TODO check wrt maps

checkSubst :: Subst -> Bool
checkSubst subst = True -- TODO does this need to be checked WRT a selector?

----- Apply a substitution to various parts of Style (relational statements, exprs, blocks)
-- Recursively walk the tree, looking up and replacing each Style variable encountered with a Substance variable
-- If a Sty var doesn't have a substitution (i.e. substitution map is bad), keep the Sty var and move on

-- TODO: factor out tree-walking code? e.g. derive Traversable
-- TODO: return "maybe" if a substitution fails?

-- TODO: remove this after beta is added
-- TODO: use correct equality comparison in Substance typechecker
-- don't compare SourcePos
predEq :: C.Predicate -> C.Predicate -> Bool
predEq p1 p2 = C.predicateName p1 == C.predicateName p2 && C.predicateArgs p1 == C.predicateArgs p2

argsEq :: Arg -> Arg -> Bool
argsEq (AVar v1) (AVar v2) = v1 == v2
argsEq (AT t1) (AT t2)     = typesEq t1 t2
argsEq _ _                 = False

typesEq :: T -> T -> Bool
typesEq (TTypeVar t1) (TTypeVar t2) = typeVarName t1 == typeVarName t2 -- TODO: better way to compare type vars
typesEq (TConstr t1) (TConstr t2) = nameCons t1 == nameCons t2 &&
                                    length (argCons t1) == length (argCons t2) &&
                                    (all (\(a1, a2) -> argsEq a1 a2) $ zip (argCons t1) (argCons t2))
typesEq _ _ = False

substituteBform :: Subst -> BindingForm -> BindingForm
substituteBform subst sv@(BSubVar _) = sv
substituteBform subst sv@(BStyVar sv'@(StyVar' vn)) =
   case M.lookup sv' subst of
   Nothing     -> sv -- error $ "No subst found for Sty var '" ++ vn ++ "'"
                  -- TODO/URGENT: no substitutions for namespaces
   Just subVar -> BSubVar subVar -- Returns result of mapping if it exists (y -> x)

substituteExpr :: Subst -> SelExpr -> SelExpr
-- theta(B) = ...
substituteExpr subst (SEBind bvar) = SEBind $ substituteBform subst bvar
-- theta(f[E]) = f([theta(E)]
substituteExpr subst (SEAppFunc fname exprs) = SEAppFunc fname $ map (substituteExpr subst) exprs
-- theta(v([E])) = v([theta(E)])
substituteExpr subst (SEAppValCons vname exprs) = SEAppValCons vname $ map (substituteExpr subst) exprs

substitutePredArg :: Subst -> PredArg -> PredArg
substitutePredArg subst (PE expr) = PE $ substituteExpr subst expr
substitutePredArg subst (PP pred) = PP $ substitutePred subst pred

substitutePred :: Subst -> Predicate -> Predicate
substitutePred subst pred = pred { predicateArgs = map (substitutePredArg subst) $ predicateArgs pred }

-- theta(|S_r) = ...
substituteRel :: Subst -> RelationPattern -> RelationPattern
-- theta(B := E) |-> theta(B) := theta(E)
substituteRel subst (RelBind bvar sExpr) = RelBind (substituteBform subst bvar) (substituteExpr subst sExpr)
-- theta(Q([a]) = Q([theta(a)])
substituteRel subst (RelPred pred) = RelPred $ substitutePred subst pred

-- Applies a substitution to a list of relational statements. theta([|S_r])
-- TODO: assumes a full substitution
substituteRels :: Subst -> [RelationPattern] -> [RelationPattern]
substituteRels subst rels = map (substituteRel subst) rels

----- Substs for the translation semantics (more tree-walking on blocks, just changing binding forms)

substitutePath :: Subst -> Path -> Path
substitutePath subst path =
    case path of
    FieldPath    bVar field      -> FieldPath    (substituteBform subst bVar) field
    PropertyPath bVar field prop -> PropertyPath (substituteBform subst bVar) field prop

substituteField :: Subst -> PropertyDecl -> PropertyDecl
substituteField subst (PropertyDecl field expr) = PropertyDecl field $ substituteBlockExpr subst expr

substituteLayering :: Subst -> LExpr -> LExpr
substituteLayering subst (LId bVar) = LId $ substituteBform subst bVar
substituteLayering subst (LPath path) = LPath $ substitutePath subst path
substituteLayering subst (LayeringOp op lex1 lex2) = 
                   LayeringOp op (substituteLayering subst lex1) (substituteLayering subst lex1)

substituteBlockExpr :: Subst -> Expr -> Expr
substituteBlockExpr subst expr =
    case expr of
    EPath path        -> EPath $ substitutePath subst path
    CompApp f es      -> CompApp f $ map (substituteBlockExpr subst) es
    ObjFn   f es      -> ObjFn   f $ map (substituteBlockExpr subst) es
    ConstrFn  f es    -> ConstrFn f $ map (substituteBlockExpr subst) es
    AvoidFn   f es    -> AvoidFn  f $ map (substituteBlockExpr subst) es
    BinOp op e1 e2    -> BinOp op (substituteBlockExpr subst e1) (substituteBlockExpr subst e2)
    UOp   op e        -> UOp   op (substituteBlockExpr subst e)
    List es           -> List $ map (substituteBlockExpr subst) es
    ListAccess path i -> ListAccess (substitutePath subst path) i
    Ctor gpi fields   -> Ctor gpi $ map (substituteField subst) fields 
    Layering lexpr    -> Layering $ substituteLayering subst lexpr
    -- No substitution for literals
    IntLit _          -> expr
    AFloat _          -> expr
    StringLit _       -> expr

substituteLine :: Subst -> Stmt -> Stmt
substituteLine subst line =
    case line of
    Assign   path expr -> Assign (substitutePath subst path) (substituteBlockExpr subst expr)
    Override path expr -> Override (substitutePath subst path) (substituteBlockExpr subst expr)
    Delete   path      -> Delete $ substitutePath subst path

-- TODO: assumes a full substitution
substituteBlock :: Subst -> Block -> Block
substituteBlock subst block = map (substituteLine subst) block

----- Filter with relational statements

-- Judgment 11. b; theta |- S <| |S_r
relMatchesLine :: C.SubEnv -> C.SubStmt -> RelationPattern -> Bool
-- rule Bind-Match
relMatchesLine subEnv (C.Bind var expr) (RelBind bvar sExpr) =
               case bvar of
               BStyVar _ -> error "Style variable found in relational statement; should not be present!"
               BSubVar sVar -> var == sVar && expr == toSubExpr sExpr -- TODO: use beta to check equality
-- rule Pred-Match
relMatchesLine subEnv (C.ApplyP pred) (RelPred sPred) = predEq pred $ toSubPred sPred
               -- TODO: use beta to check for implication
relMatchesLine _ _ _ = False -- no other line forms match (decl, equality, etc.)

-- Judgment 13. b |- [S] <| |S_r
relMatchesProg :: C.SubEnv -> C.SubProg -> RelationPattern -> Bool
relMatchesProg subEnv subProg rel = any (flip (relMatchesLine subEnv) rel) subProg

-- Judgment 15. b |- [S] <| [|S_r]
allRelsMatch :: C.SubEnv -> C.SubProg -> [RelationPattern] -> Bool
allRelsMatch subEnv subProg rels = all (relMatchesProg subEnv subProg) rels

-- Judgment 17. b; [theta] |- [S] <| [|S_r] ~> [theta']
-- Folds over [theta]
filterRels :: C.SubEnv -> C.SubProg -> [RelationPattern] -> [Subst] -> [Subst]
filterRels subEnv subProg rels substs = 
           filter (\subst -> allRelsMatch subEnv subProg (substituteRels subst rels)) substs

----- Match declaration statements

-- Judgment 9. G; theta |- T <| |T
-- Assumes types are nullary, so doesn't return a subst, only a bool indicating whether the types matched
matchType :: VarEnv -> T -> StyT -> Bool
matchType varEnv (TConstr tctor) (STCtor stctor) =
          if length (argCons tctor) > 0 || length (argConsS stctor) > 0
          then error "no types with parameters allowed in match" -- TODO error msg
          else trM1 ("types: " ++ nameCons tctor ++ ", " ++ nameConsS stctor) $ 
               nameCons tctor == nameConsS stctor -- TODO subtyping
-- TODO better errors + think about cases below
matchType varEnv (TTypeVar tvar) (STTypeVar stvar) = error "no type vars allowed in match" 
matchType varEnv (TConstr tvar) (STTypeVar stvar) = error "no type vars allowed in match"
matchType varEnv (TTypeVar tvar) (STCtor stctor) = error "no type vars allowed in match"

-- Judgment 10. theta |- x <| B
matchBvar :: Var -> BindingForm -> Maybe Subst
matchBvar subVar (BStyVar styVar) = Just $ M.insert styVar subVar M.empty
matchBvar subVar (BSubVar styVar) = if subVar == styVar 
                                    then Just M.empty 
                                    else Nothing

-- Judgment 12. G; theta |- S <| |S_o
matchDeclLine :: VarEnv -> C.SubStmt -> DeclPattern -> Maybe Subst
matchDeclLine varEnv (C.Decl subT subVar) (PatternDecl' styT bvar) =
              let typesMatched = matchType varEnv subT styT in
              if typesMatched
              then trM1 "types matched" $ matchBvar subVar bvar
              else trM1 "types didn't match" $ Nothing -- substitution is only valid if types matched first
matchDeclLine _ subL styL = Nothing -- Sty decls only match Sub decls
          
-- Judgment 16. G; [theta] |- [S] <| [|S_o] ~> [theta']
matchDecl :: VarEnv -> C.SubProg -> [Subst] -> DeclPattern -> [Subst]
matchDecl varEnv subProg initSubsts decl = 
          -- Judgment 14. G; [theta] |- [S] <| |S_o
          let newSubsts = map (flip (matchDeclLine varEnv) decl) subProg in
          trM1 ("new substs: " ++ show newSubsts) $ merge initSubsts (catMaybes newSubsts)
          -- TODO: why is this trace necessary to see the rest of the debug output? 
          -- is it because of list comprehensions?

-- Judgment 18. G; [theta] |- [S] <| [|S_o] ~> [theta']
-- Folds over [|S_o]
matchDecls :: VarEnv -> C.SubProg -> [DeclPattern] -> [Subst] -> [Subst]
matchDecls varEnv subProg decls initSubsts = foldl (matchDecl varEnv subProg) initSubsts decls

----- Overall judgments

find_substs_sel :: VarEnv -> C.SubEnv -> C.SubProg -> Header -> [Subst]
-- Judgment 19. G; b; [theta] |- [S] <| Sel
find_substs_sel varEnv subEnv subProg (Select sel) =
    let decls            = selHead sel ++ selWith sel
        rels             = selWhere sel
        initSubsts       = []
        subst_candidates = matchDecls varEnv subProg decls initSubsts
        -- TODO: check validity of subst_candidates (all StyVars have exactly one SubVar)
        filtered_substs  = trM1 ("candidates: " ++ show subst_candidates) $ 
                           filterRels subEnv subProg rels subst_candidates
        correct_substs   = filter checkSubst filtered_substs
    in correct_substs
find_substs_sel _ _ _ (Namespace _) = [] -- No substitutions for a namespace (not in paper)

-- TODO: add note on prog, header judgment to paper?
-- Find a list of substitutions for each selector in the Sty program.
find_substs_prog :: VarEnv -> C.SubEnv -> C.SubProg -> StyProg -> [[Subst]]
find_substs_prog varEnv subEnv subProg styProg =
    let sels = map fst styProg in
    map (find_substs_sel varEnv subEnv subProg) sels

-- TODO: should Sub:[Scalar x, y] Sty:[Scalar c, d] generate all 4 matches? what is the intent here?
-- TODO: make anonymous variables for unification for Substance program

-------------------- Translation dynamics (i.e. actual Style compiler)

-- TODO: block statics
checkBlock :: SelEnv -> Block -> [Error]
checkBlock selEnv block = []

----- Type definitions

newtype SField = Field' String
    deriving (Show, Eq, Typeable)

newtype SProperty = Prop' String
    deriving (Show, Eq, Typeable)

-- TODO: add lists to S.TypeIn
-- TODO: S.TypeIn doesn't support objfns etc

data GPICtor = Ellip | Circle | Box | Rectangle | Dot | Arrow | NoShape | Color | Text | Curve | Auto
               | Arc2 | Line2 | Parallel | Image | AnchorPoint | CurlyBrace
    deriving (Show, Eq, Ord, Typeable) -- Ord for M.toList in Runtime

data TagExpr a = OptEval Expr      -- Thunk evaluated at each step of optimization-time
               | Done (S.TypeIn a) -- A value in the host language, fully evaluated
    deriving (Show, Eq, Typeable)

-- Should we use the Property/Field newtypes?
type PropertyDict a = M.Map Property (TagExpr a)
type FieldDict a = M.Map Field (FieldExpr a)

data FieldExpr a = FExpr (TagExpr a) 
                 | GPI GPICtor (PropertyDict a)
    deriving (Show, Eq, Typeable)

type Warning = String
data Name = Sub String   -- Sub obj name
            | Gen String -- randomly generated name
    deriving (Show, Eq, Ord, Typeable)

data Translation a = Trans { trMap    :: M.Map Name (FieldDict a),
                             warnings :: [Warning] }
    deriving (Show, Eq, Typeable)

-- For a Substance object "A", the Translation might look like this:
-- Trans [ "A" =>
--        FieldDict [ "val"   => FExpr (Done (IntLit 2)), 
--                    "shape" => GPI Circ PropMap [ "r" => OptEval (EPath (FieldPath (SubVar (VC "B"))) "val"),
--                                                  "x" => ...  ] ] ]

type OverrideFlag = Bool

----- Translation util functions

initTrans :: Autofloat a => Translation a
initTrans = Trans { trMap = M.empty, warnings = [] }

-- Convert Sub bvar name to Sub name in Translation
trName :: BindingForm -> Name
trName (BSubVar (VarConst nm)) = Sub nm
trName (BStyVar (StyVar' nm))  = Sub nm -- error ("Style variable '" ++ show bv ++ "' in block! Was a non-full substitution applied?") -- TODO/URGENT fix for namespaces

nameStr :: Name -> String
nameStr (Sub s) = s
nameStr (Gen s) = s

-- TODO: replace this with styObj parser
toCtorType :: String -> GPICtor
toCtorType "Color"   = Color
toCtorType "None"    = NoShape
toCtorType "Arrow"   = Arrow
toCtorType "Text"    = Text
toCtorType "Circ"  = Circle -- TODO: Circle parsing???
toCtorType "Curve"   = Curve
toCtorType "Ellipse" = Ellip
toCtorType "Box"     = Box
toCtorType "Arc"     = Arc2
toCtorType "Rectangle"    = Rectangle
toCtorType "Parallelogram" = Parallel
toCtorType "Dot"     = Dot
toCtorType "Line"    = Line2
toCtorType "Image"   = Image
toCtorType "AnchorPoint"   = AnchorPoint
toCtorType "CurlyBrace"   = CurlyBrace
toCtorType s         = error ("Unrecognized shape: " ++ s)

mkPropertyDict :: (Autofloat a) => [PropertyDecl] -> PropertyDict a
mkPropertyDict propertyDecls = foldl addPropertyDecl M.empty propertyDecls
    where addPropertyDecl :: PropertyDict a -> PropertyDecl -> PropertyDict a
          -- TODO: check that the same property is not declared twice
          addPropertyDecl dict (PropertyDecl property expr) = M.insert property (OptEval expr) dict

-- All warnings are appended
addMaybe :: [a] -> Maybe a -> [a]
addMaybe xs x = xs ++ maybeToList x

addMaybes :: [a] -> [Maybe a] -> [a]
addMaybes xs ms = xs ++ catMaybes ms

addWarn :: Translation a -> Warning -> Translation a
addWarn tr warn = tr { warnings = warnings tr ++ [warn] }

pathStr :: Name -> Field -> Property -> String
pathStr name field property = intercalate "." [nameStr name, field, property]

----- Main operations on translations (add and delete)

-- TODO distinguish between warns/errs
deleteField :: (Autofloat a) => Translation a -> Name -> Field -> Translation a
deleteField trans name field =
    let trn = trMap trans in
    case M.lookup name trn of
    Nothing ->
        let err = "Err: Sub obj '" ++ nameStr name ++ "' has no fields; can't delete field '" ++ field ++ "'" in
        addWarn trans err
    Just fieldDict -> 
        if field `M.notMember` fieldDict
        then let warn = "Warn: Sub obj '" ++ nameStr name ++ "' already lacks field '" ++ field ++ "'" in 
             addWarn trans warn
        else let fieldDict' = M.delete field fieldDict
                 trn'       = M.insert name fieldDict' trn in
             trans { trMap = trn' }

deleteProperty :: (Autofloat a) => Translation a -> Name -> Field -> Property -> Translation a
deleteProperty trans name field property =
    let trn = trMap trans
        path = pathStr name field property in
    case M.lookup name trn of
    Nothing ->
        let err = "Err: Sub obj '" ++ nameStr name ++ "' has no fields; can't delete path '" ++ path ++ "'" in
        addWarn trans err
    Just fieldDict -> 
        case M.lookup field fieldDict of
        Nothing -> let err = "Err: Sub obj '" ++ nameStr name ++ "' already lacks field '" ++ field 
                              ++ "'; can't delete path " ++ path in 
                   addWarn trans err
        Just (FExpr _) -> let err = "Error: Sub obj '" ++ nameStr name ++ "' does not have GPI '" 
                                     ++ field ++ "'; cannot delete property '" ++ property ++ "'" in
                          addWarn trans err
        Just (GPI ctor properties) ->
           -- If the field is GPI, check if property already exists
           if property `M.notMember` properties
           then let warn = "Warning: property '" ++ property ++ "' already does not exist in path '" 
                           ++ pathStr name field property ++ "'; deletion does nothing"
                in addWarn trans warn
           else let properties' = M.delete property properties
                    fieldDict'  = M.insert field (GPI ctor properties') fieldDict
                    trn'        = M.insert name fieldDict' trn in
                trans { trMap = trn' }

-- Implements two rules for fields:
-- x.n = Ctor { n_i = e_i }, rule Line-Set-Ctor, for GPI
-- x.n = e, rule Line-Set-Field-Expr
addField :: (Autofloat a) => OverrideFlag -> Translation a -> 
                             Name -> Field -> Expr -> Translation a
addField override trans name field expr =
    let trn = trMap trans in
    let fieldDict = case M.lookup name trn of
                    Nothing    -> M.empty -- Initialize the field dict if it hasn't been initialized
                    Just fdict -> fdict in
     -- Warn using override if x doesn't exist
    let warn1 = if fieldDict == M.empty && override
                then Just $ "Warning: Sub obj '" ++ nameStr name ++ "' has no fields, but override was declared"
                else Nothing in
     -- Warn using override if x.n already exists
    let warn2 = if (field `M.member` fieldDict) && (not override)
                then Just $ "Warning: Sub obj '" ++ nameStr name ++ "''s field '" ++ field 
                            ++ "' is overridden, but was not declared an override"
                else Nothing in
     -- Warn using override if x.n doesn't exist
    let warn3 = if (field `M.notMember` fieldDict) && override
                then Just $ "Warning: field '" ++ field ++ "' declared override, but has not been initialized"
                else Nothing in
    -- TODO: check existing FExpr is overridden by an FExpr and likewise for Ctor of same type (typechecking)
    let fieldExpr = case expr of
                    Ctor ctorName propertyDecls -> -- rule Line-Set-Ctor
                         GPI (toCtorType ctorName) (mkPropertyDict propertyDecls)
                    _ -> FExpr (OptEval expr) in   -- rule Line-Set-Field-Expr
    let fieldDict' = M.insert field fieldExpr fieldDict
        trn' = M.insert name fieldDict' trn in
    trans { trMap = trn', warnings = addMaybes (warnings trans) [warn1, warn2, warn3] }

addProperty :: (Autofloat a) => OverrideFlag -> Translation a ->
                                Name -> Field -> Property -> Expr -> Translation a
addProperty override trans name field property val =
    let trn = trMap trans in
    -- Setting a field's property should require that field to already exist and be a GPI
    -- TODO: distinguish b/t errors and warns
    case M.lookup name trn of
    Nothing -> let err = "Error: Sub obj '" ++ nameStr name ++ "' has no fields; cannot add property" in
               addWarn trans err
    Just fieldDict ->
        case M.lookup field fieldDict of
        Nothing -> let err = "Error: Sub obj '" ++ nameStr name ++ "' does not have field '" 
                              ++ field ++ "'; cannot add property '" ++ property ++ "'" in
                   addWarn trans err
        Just (FExpr _) -> let err = "Error: Sub obj '" ++ nameStr name ++ "' does not have GPI '" 
                                     ++ field ++ "'; cannot add property '" ++ property ++ "'" in
                          addWarn trans err
        Just (GPI ctor properties) ->
           -- If the field is GPI, check if property already exists and whether it matches the override setting
           let warn = if (property `M.notMember` properties) && override 
                      then Just $ "Warning: property '" ++ property ++ "' does not exist in path '" 
                           ++ pathStr name field property ++ "' but override was set"
                      else if property `M.member` properties && (not override)
                      then Just $ "Warning: property '" ++ property ++ "' already exists in path '" 
                           ++ pathStr name field property ++ "' but override was not set"
                      else Nothing in
           let properties' = M.insert property (OptEval val) properties
               fieldDict'  = M.insert field (GPI ctor properties') fieldDict
               trn'        = M.insert name fieldDict' trn in
           trans { trMap = trn', warnings = addMaybe (warnings trans) warn }

-- rule Line-Delete
deletePath :: (Autofloat a) => Translation a -> Path -> Either [Error] (Translation a)
deletePath trans path =
    case path of
    FieldPath bvar field ->
        let name = trName bvar
            trans' = deleteField trans name field in
        Right trans'
    PropertyPath bvar field property ->
       let name = trName bvar
           trans' = deleteProperty trans name field property in
       Right trans'

addPath :: (Autofloat a) => OverrideFlag -> Translation a -> Path -> Expr -> Either [Error] (Translation a)
addPath override trans path expr =
    case path of
    -- rule Line-Set-Field-Expr, Line-Set-Ctor
    FieldPath bvar field ->
        let name   = trName bvar
            trans' = addField override trans name field expr in
        Right trans'
    -- rule Line-Set-Prop-Expr
    PropertyPath bvar field property ->
       let name   = trName bvar
           trans' = addProperty override trans name field property expr in
       Right trans'

----- Translation judgments

-- Note: All of the folds below use foldM.
-- foldM stops accumulating when the first fatal error is reached, using "Either [Error]" as a monad
-- (Non-fatal errors are stored as warnings in the translation)
-- foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
-- example: 
-- f acc elem = if elem < 0 then Left ["wrong " ++ show elem] else Right $ elem : acc
-- foldM f [] [1, 9, -1, 2, -2] = Left ["wrong -1"]
-- foldM f [] [1, 9] = Right [9,1]

-- Judgment 26. D |- phi ~> D'
-- This is where interesting things actually happen (each line is interpreted and added to the translation)
translateLine :: (Autofloat a) => Translation a -> Stmt -> Either [Error] (Translation a)
translateLine trans stmt =
    case stmt of
    Assign path expr   -> addPath False trans path expr
    Override path expr -> addPath True trans path expr
    Delete path        -> deletePath trans path

-- Judgment 25. D |- |B ~> D' (modified to be: theta; D |- |B ~> D')
translateBlock :: (Autofloat a) => Block -> Translation a -> Subst -> Either [Error] (Translation a)
translateBlock block trans subst =
    let block' = substituteBlock subst block in
    foldM translateLine trans block'

-- Judgment 24. [theta]; D |- |B ~> D'
translateSubstsBlock :: (Autofloat a) => Translation a -> [Subst] -> Block -> Either [Error] (Translation a)
translateSubstsBlock trans substs block = foldM (translateBlock block) trans substs

-- Judgment 23, contd.
translatePair :: (Autofloat a) => VarEnv -> C.SubEnv -> C.SubProg -> 
                                  Translation a -> (Header, Block) -> Either [Error] (Translation a)
translatePair varEnv subEnv subProg trans (Namespace styVar, block) =
    let selEnv = initSelEnv
        bErrs  = checkBlock selEnv block in
    if null (sErrors selEnv) && null bErrs
        then let subst = M.empty in -- is this the correct empty?
             translateBlock block trans subst -- skip transSubstsBlock
        else Left $ sErrors selEnv ++ bErrs
    -- TODO translate namespaces properly?

translatePair varEnv subEnv subProg trans (header@(Select sel), block) =
    let selEnv = checkSel varEnv sel
        bErrs  = checkBlock selEnv block in
    if null (sErrors selEnv) && null bErrs
        then let substs = find_substs_sel varEnv subEnv subProg header in
             translateSubstsBlock trans substs block
        else Left $ sErrors selEnv ++ bErrs
            
-- TODO: add beta in paper and to comment below
-- Judgment 23. G; D |- [P]; |P ~> D'
-- Fold over the pairs in the Sty program, then the substitutions for a selector, then the lines in a block.
translateStyProg :: (Autofloat a) => VarEnv -> C.SubEnv -> C.SubProg -> StyProg -> Either [Error] (Translation a)
translateStyProg varEnv subEnv subProg styProg =
                 foldM (translatePair varEnv subEnv subProg) initTrans styProg
                 -- TODO: deal with warnings in translation
                 -- TODO: remember to check the order of warnings (reverse?)

------------ Translation second pass to actually make GPIs, objectives, computations

-- TODO: filter Translation

-- TODO: interpret and eval should use the State and TrComputations
-- [|e|] : evaluate an expression as much as possible at compile-time
-- stopping when an optimized value is encountered
-- TODO this should call eval
interpret :: (Autofloat a) => Expr -> TagExpr a
interpret e = Done $ S.TStr "TODO" -- FILL IN

eval :: TagExpr a -> TagExpr a
eval e@(Done e') = e
eval e@(OptEval e') = e -- FILL IN
