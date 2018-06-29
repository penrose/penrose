-- | The "Style" module contains the compiler for the Style language,
-- and functions to traverse the Style AST, which are used by "Runtime"

{-# OPTIONS_HADDOCK prune #-}
module NewStyle where
-- module Main (main) where -- for debugging purposes

-- import Shapes
import Utils
import Control.Monad (void)
import Data.Function (on)
import Data.Either (partitionEithers)
import Data.Either.Extra (fromLeft)
import Data.Maybe (fromMaybe, catMaybes)
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
    = FieldPath BindingForm Field
    | PropertyPath BindingForm Field Property
    deriving (Show, Eq, Typeable)

-- | A statement in the Style language
data Stmt
    = Assign Path Expr
    | Override Path Expr
    | Delete Expr
    deriving (Show, Eq, Typeable)

-- | A field declaration in a Style constructor binds an expression to a
-- string
data FieldDecl = FieldDecl String Expr
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
    | Ctor String [FieldDecl]
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
delete   = Delete   <$> (rword "delete"   >> expr)

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
    fields <- braces (fieldDecl `sepEndBy` newline')
    return $ Ctor typ fields

fieldDecl :: Parser FieldDecl
fieldDecl = FieldDecl <$> identifier <*> (eq >> expr)

stringLit :: Parser Expr
stringLit = StringLit <$> (char '"' >> manyTill L.charLiteral (char '"'))

annotatedFloat :: Parser AnnoFloat
annotatedFloat = (rword "OPTIMIZED" *> pure Vary) <|> Fix <$> float

------------------------------------------------------------------------
-- Style static semantics for selectors

------------ Type defs

-- g ::= B => |T
-- Assumes nullary type constructors (i.e. Style type = Substance type)
-- TODO: hold error in env?
data SelEnv = SelEnv { sTypeVarMap :: M.Map BindingForm StyT, -- x : T
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
toSubExpr (SEAppFunc f exprs) = C.ApplyExpr $ C.Func { C.nameFunc = f, C.argFunc = map toSubExpr exprs }
-- TODO: after Substance merge/revision, convert to use val ctor branch of Expr
toSubExpr (SEAppValCons v exprs) = C.ApplyExpr $ C.Func { C.nameFunc = v, C.argFunc = map toSubExpr exprs }

toSubPredArg :: PredArg -> C.PredArg
toSubPredArg (PE selExpr) = C.PE $ toSubExpr selExpr
toSubPredArg (PP pred)    = C.PP $ toSubPred pred

toSubPred :: Predicate -> C.Predicate
toSubPred stypred = C.Predicate { C.predicateName = C.PredicateConst $ predicateName stypred, 
                                  C.predicateArgs = map toSubPredArg $ predicateArgs stypred, 
                                  C.predicatePos = predicatePos stypred }

----------- Selector static semantics

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
     if vtype' == etype' then Nothing
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
                      -- G |- B : T1
                      let (error1, vtype) = C.checkVarE varEnv $ toSubVar bVar in
                      -- G |- E : T2
                      let (error2, etype) = C.checkExpression varEnv $ toSubExpr sExpr in
                      -- T1 = T2
                      let err3 = compareTypes rel bVar sExpr vtype etype in
                      case err3 of 
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
                     -- y \not\in dom(g)
                     if M.member bsv (sTypeVarMap selEnv')
                     then let err = "Style pattern statement " ++ show stmt ++ 
                                    " declares Style variable '" ++ styVar ++ "' twice" in
                          addErr err selEnv'
                     else addMapping bsv styType selEnv'
             -- rule Decl-Sub-Context
             bsv@(BSubVar subVar) -> 
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
                                         "in Substance but has type '" ++ show declType ++ "' in Style." in
                              addErr err selEnv'

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
-- TODO: add these judgments
checkSels :: VarEnv -> StyProg -> [SelEnv]
checkSels varEnv prog = map (checkPair varEnv) prog
          where checkPair :: VarEnv -> (Header, Block) -> SelEnv
                checkPair varEnv ((Select sel), _) = checkSel varEnv sel
                checkPair varEnv ((Namespace name), _) = initSelEnv 
                -- TODO: for now, namespace has no local context 

-- TODO: write test cases (including failing ones)
-- TODO: get line/col numbers for errors
