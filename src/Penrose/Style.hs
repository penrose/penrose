-- | The "Style" module contains the compiler for the Style language,
-- and functions to traverse the Style AST.

{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, UnicodeSyntax, NoMonomorphismRestriction, FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- Mostly for autodiff

module Penrose.Style where

import           Control.Applicative              ((<**>))
import           Control.Applicative.Permutations
import           Control.Monad                    (foldM, void)
import           Control.Monad.Combinators.Expr
import           Control.Monad.State.Lazy         (evalStateT, get)
import           Data.Dynamic
import           Data.Either                      (partitionEithers)
import           Data.Either.Extra                (fromLeft)
import           Data.Function                    (on)
import           Data.List                        (intercalate, nub, nubBy,
                                                   partition, sort)
import qualified Data.Map.Strict                  as M
import           Data.Maybe                       (catMaybes, fromMaybe,
                                                   isNothing, maybeToList)
import           Data.Tuple                       (swap)
import           Data.Typeable
import           Debug.Trace
import           Penrose.Env
import           Penrose.Functions
import           Penrose.Shapes                   hiding (get)
import qualified Penrose.Substance                as C
import           Penrose.SubstanceJSON            as J
import           Penrose.Util
import           System.Environment
import           System.Random
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as L
import           Text.Show.Pretty                 (ppShow)


--------------------------------------------------------------------------------
-- Plugin Parser

-- | A Style program can have an instantiator, which would expand its corresponding Substance program via an external program. The instantiator is specified with a string
-- NOTE: for now, we would allow multiple instantiation statements, but only use the __first__ instantiator in the Style program
type PluginName = String
data Plugin  = Plugin PluginName [Expr] 
type Plugins = [Plugin]

-- | 'parsePlugins' parses a Style program and return a list of instantiators declared at the __top__ of the program.
-- NOTE: this parse do run the Style parser to check for ill-formed instantiation statements. Therefore, a 'VarEnv' is required to run this parser without any errors
parsePlugins :: String -> String -> VarEnv -> Either CompilerError Plugins
parsePlugins styFile styIn env =
    case runParser (pluginParser env) styFile styIn of
    Left err -> Left $ PluginParse (errorBundlePretty err)
    Right instantiation -> Right instantiation

pluginParser :: VarEnv -> BaseParser Plugins
pluginParser env = evalStateT pluginParser' $ Just env

pluginParser' :: Parser Plugins
pluginParser' = do
    scn
    res <- plugins
    _   <- styProg
    return res
    where restOfFile = manyTill anySingle eof >> return []

plugins :: Parser Plugins
plugins = plugin `endBy` newline' -- zero or multiple instantiators

plugin :: Parser Plugin
plugin = symbol "plugin" >> (Plugin <$> stringLiteral <*> option [] exprsInParens)

stringLiteral :: Parser String
stringLiteral = symbol "\"" >> manyTill L.charLiteral (try (symbol "\""))

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
-- TODO: write a NOTE about the namespace situation between Substance and Style.

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
    deriving (Show, Eq, Typeable, Ord)

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
data Expr
    = IntLit Int
    | AFloat AnnoFloat
    | StringLit String
    | BoolLit Bool
    | EPath Path
    | CompApp String [Expr]
    | ObjFn String [Expr]
    | ConstrFn String [Expr]
    | AvoidFn String [Expr]  -- TODO: to be implemented
    | BinOp BinaryOp Expr Expr
    | UOp UnaryOp Expr
    | List [Expr]
    | Tuple Expr Expr
    | ListAccess Path Integer
    | Ctor String [PropertyDecl] -- Shouldn't be using this, since we have PropertyDict
    | Layering Path Path -- ^ first GPI is *below* the second GPI
    | PluginAccess String Expr Expr -- ^ Plugin name, Substance name, Key
    | ThenOp Expr Expr -- COMBAK: double check how transforms are modeled, probably just a list of CompApp
    deriving (Show, Eq, Typeable)

data LayerOp  = Less | Seq
    deriving (Show, Eq, Typeable)

data UnaryOp  = UPlus | UMinus
    deriving (Show, Eq, Typeable)

data BinaryOp = BPlus | BMinus | Multiply | Divide | Exp
    deriving (Show, Eq, Typeable)

--------------------------------------------------------------------------------
-- Style Parser

-- | 'parseStyle' runs the actual parser function: 'styleParser', taking in a program String and parse it into an AST.
parseStyle :: String -> String -> VarEnv -> Either CompilerError StyProg
parseStyle styFile styIn env =
  case runParser (styleParser env) styFile styIn of
    Left err  -> Left $ StyleParse $ (errorBundlePretty err)
    Right res -> Right res

-- | 'styleParser' is the top-level function that parses a Style proram
styleParser :: VarEnv -> BaseParser StyProg
styleParser env = evalStateT styleParser' $ Just env
styleParser' = between scn eof styProg

-- | `styProg` parses a Style program, consisting of a collection of one or
-- more blocks
styProg :: Parser StyProg
styProg =
    plugins >>  -- ignore plugin statements
    some headerBlock
    where headerBlock = (,) <$> header <*> braces block <* scn

header :: Parser Header
header = tryChoice [Select <$> selector, Namespace <$> styVar]

-------------------- Selector parsers
-- COMBAK: short-hand for decls with the same type
-- COMBAK: change commas to semicolons

-- TODO: clear up the `scn` calls for all parsers and establish the convention of calling scn AFTER each parser
selector :: Parser Selector
selector = do
    hd       <- fmap concat $ declPattern `sepBy1` semi' <* scn
    (wi, wh) <- withAndWhere
    ns       <- optional $ namespace <* scn
    return Selector { selHead = hd,  selWith = wi, selWhere = wh,
                      selNamespace = ns}
    where wth = fmap concat $ rword "with" *> declPattern `sepEndBy1` semi' <* scn
          whr = rword "where" *> relationPattern `sepEndBy1` semi' <* scn
          namespace = rword "as" >> identifier
          withAndWhere = runPermutation $ (,) <$> toPermutationWithDefault [] wth <*> toPermutationWithDefault [] whr

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
    pos <- getSourcePos
    return STypeVar' { typeVarNameS = t, typeVarPosS = pos}

typeConstructor :: Parser STypeCtor
typeConstructor = do
    n    <- identifier
    args <- option [] $ parens (styArgument `sepBy1` comma)
    pos  <- getSourcePos
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
    pos  <- getSourcePos
    return Predicate { predicateName = n, predicateArgs = args,
                       predicatePos = pos }

predicateArgument :: Parser PredArg
predicateArgument = tryChoice [PP <$> predicate, PE <$> selectorExpr]

selectorExpr :: Parser SelExpr
selectorExpr =
        try selectorValConsOrFunc <|>
        SEBind       <$> bindingForm

bindingForm :: Parser BindingForm
bindingForm = BSubVar <$> backticks var <|> BStyVar <$> styVar

-- NOTE: this is a duplication of "valConsOrFunc" in Substance parser, with selector specific types
selectorValConsOrFunc :: Parser SelExpr
selectorValConsOrFunc = do
    n <- identifier
    e <- get
    let env = fromMaybe (error "Style parser: variable environment is not intiialized.") e
    args <- parens (selectorExpr `sepBy1` comma)
    case (M.lookup n $ valConstructors env, M.lookup n $ operators env) of
        -- the id is a value constructor
        (Just _, Nothing)  -> return $ SEAppValCons n args
        -- the id is an operator
        (Nothing, Just _)  -> return $ SEAppFunc n args
        (Nothing, Nothing) -> styleErr $ "undefined identifier " ++ n
        _ -> styleErr $  n ++ " cannot be both a value constructor and an operator"
    where styleErr s = customFailure (StyleError s)

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
           layeringExpr,
           arithmeticExpr,
           objFn,
           constrFn,
           transformExpr, -- COMBAK: ordering
           compFn,
           list,
           tuple,
           stringLit,
           boolLit
       ]

pluginAccess :: Parser Expr
pluginAccess = do
    plugin <- identifier
    name   <- bExpr
    key    <- bExpr
    return $ PluginAccess plugin name key
    where bExpr = brackets expr

arithmeticExpr :: Parser Expr
arithmeticExpr = makeExprParser aTerm aOperators

aTerm :: Parser Expr
aTerm = tryChoice
    [
        compFn,
        pluginAccess,
        parens arithmeticExpr,
        AFloat <$> annotatedFloat,
        EPath  <$> path,
        IntLit . fromIntegral <$> integer
    ]

aOperators :: [[Control.Monad.Combinators.Expr.Operator Parser Expr]]
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

layeringExpr :: Parser Expr
layeringExpr = try layeringAbove <|> layeringBelow
    where
        layeringBelow = Layering <$> path <* rword "below" <*> path
        layeringAbove = do
            path1 <- path
            rword "above"
            path2 <- path
            return $ Layering path2 path1

transformExpr :: Parser Expr
transformExpr = makeExprParser tTerm tOperators

tTerm :: Parser Expr
tTerm = compFn

tOperators :: [[Control.Monad.Combinators.Expr.Operator Parser Expr]]
tOperators =
    [   -- Highest precedence
        [ InfixL (ThenOp <$ symbol "then") ]
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

exprsInParens :: Parser [Expr]
exprsInParens = parens $ expr `sepBy` comma

list, tuple :: Parser Expr
list = List <$> brackets (expr `sepBy1` comma)
tuple = parens (Tuple <$> expr <*> (comma >> expr))

constructor :: Parser Expr
constructor = do
    typ    <- identifier
    fields <- braces (propertyDecl `sepEndBy` newline')
    return $ Ctor typ fields

propertyDecl :: Parser PropertyDecl
propertyDecl = PropertyDecl <$> identifier <*> (colon >> expr)

boolLit :: Parser Expr
boolLit =  (rword "True" >> return (BoolLit True))
       <|> (rword "False" >> return (BoolLit False))

stringLit :: Parser Expr
-- NOTE: overlapping parsers 'charLiteral' and 'char '"'', so we use 'try'
stringLit = StringLit <$> (symbol "\"" >> manyTill L.charLiteral (try (symbol "\"")))

annotatedFloat :: Parser AnnoFloat
annotatedFloat = (question *> pure Vary) <|> Fix <$> float

------------------------------------------------------------------------
-------- STYLE COMPILER

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

-- | Convert a Style type to a Substance type. (This has nothing to do with subtyping)
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
checkRelPatterns varEnv rels = concatMap (checkRelPattern varEnv) rels
    -- Judgment 4. G |- |S_r ok
    where checkRelPattern :: VarEnv -> RelationPattern -> [Error]
          checkRelPattern varEnv rel =
              case rel of
              -- rule Bind-Context
              RelBind bVar sExpr ->
                      -- TODO: use checkSubStmt here (and in paper)?
                      -- TODO: make sure the ill-typed bind selectors fail here (after Sub statics is fixed)
                      -- G |- B : T1
                      let (error1, vtype) = C.checkVarE varEnv
                                            (trM2 ("B: " ++ show (toSubVar bVar)) $ toSubVar bVar) in
                      -- G |- E : T2
                      let (error2, etype) = C.checkExpression varEnv
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
                     -- NOTE: this does not aggregate *all* possible error May just return first error.
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
                             -- check "T <: |T", assuming type constructors are nullary
                             let declType = toSubType styType in
                             if subType' == declType
                                || isSubtype subType' declType varEnv
                             then addMapping bsv styType selEnv'
                             else let err = "Mismatched types between Substance and Style var\n" ++
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

checkNamespace :: String -> VarEnv -> SelEnv
checkNamespace name varEnv =
    -- A Substance variable cannot have the same name as a namespace
    let error = if M.member (VarConst name) (varMap varEnv)
                then ["Substance variable '" ++ name ++ "' has the same name as a namespace in the Style program"]
                else []
    in addErrs error initSelEnv

-- Returns a sel env for each selector in the Style program, in the same order
-- TODO: add these judgments to the paper
checkSels :: VarEnv -> StyProg -> [SelEnv]
checkSels varEnv prog =
          let selEnvs = map (checkPair varEnv) prog in
          let errors = concatMap sErrors selEnvs in
          if null errors
          then selEnvs
          else error $ intercalate "\n" errors
          where checkPair :: VarEnv -> (Header, Block) -> SelEnv
                checkPair varEnv (Select sel, _) = checkSel varEnv sel
                checkPair varEnv (Namespace (StyVar' name), _) = checkNamespace name varEnv
                -- TODO: for now, namespace has no local context

-- TODO: namespaces are implicitly converted to Substance variables somewhere (not sure where)
-- TODO: write test cases (including failing ones) after Substance errors are no longer @ runtime
-- TODO: get line/col numbers for errors

----------- Selector dynamic semantics (matching)

----- Type declarations

-- A substitution θ has form [y → x], binding Sty vars to Sub vars (currently not expressions).
type Subst = M.Map StyVar Var

type LocalVarId = (Int, Int)
-- Index of the block, paired with the index of the current substitution
-- Should be unique across blocks and substitutions

localKeyword :: String
localKeyword = "LOCAL"

mkLocalVarName :: LocalVarId -> String
mkLocalVarName (blockNum, substNum) = localKeyword ++ "_b" ++ show blockNum ++ "_s" ++ show substNum

----- Substitution helper functions

-- (+) operator combines two substitutions: subst -> subst -> subst
combine :: Ord k => M.Map k a -> M.Map k a -> M.Map k a
combine s1 s2 = M.union s1 s2
-- TODO check for duplicate keys (and vals)

-- (x) operator combines two lists of substitutions: [subst] -> [subst] -> [subst]
-- the way merge is used, I think each subst in the second argument only contains one mapping
merge :: Ord k => [M.Map k a] -> [M.Map k a] -> [M.Map k a]
merge s1 [] = s1
merge [] s2 = s2
merge s1 s2 = [ combine s1i s2j | s1i <- s1, s2j <- s2 ] -- TODO check wrt maps

allUnique :: Eq a => [a] -> Bool
allUnique l = nub l == l

isStyVar :: BindingForm -> Bool
isStyVar (BStyVar _) = True
isStyVar (BSubVar _) = False

-- Judgment 20. A substitution for a selector is only correct if it gives exactly one
--   mapping for each Style variable in the selector.
fullSubst :: SelEnv -> Subst -> Bool
fullSubst selEnv subst =
    let selStyVars = map styVarToString $ filter isStyVar $ M.keys $ sTypeVarMap selEnv
        substStyVars = map styVarToString' $ M.keys subst
        res = sort selStyVars == sort substStyVars in
    -- Equal up to permutation (M.keys ensures that there are no dups)
    trM1 ("fullSubst: \nselEnv: " ++ ppShow selEnv ++ "\nsubst: " ++ ppShow subst ++ "\nres: " ++ ppShow res ++ "\n") res
    where styVarToString  (BStyVar (StyVar' v)) = v
          styVarToString' (StyVar' v) = v

-- Check that there are no duplicate keys or vals in the substitution
uniqueKeysAndVals :: Subst -> Bool
uniqueKeysAndVals subst =
    let (keys, vals) = unzip $ M.toList subst in
    allUnique keys && allUnique vals
    -- if not (allUnique keys && allUnique vals)
    -- then error ("substitution contains some duplicated keys or vals:\n" ++ show keys ++ "\n" ++ show vals)
    -- else True

----- Apply a substitution to various parts of Style (relational statements, exprs, blocks)
-- Recursively walk the tree, looking up and replacing each Style variable encountered with a Substance variable
-- If a Sty var doesn't have a substitution (i.e. substitution map is bad), keep the Sty var and move on

-- TODO: return "maybe" if a substitution fails?

substituteBform :: Maybe LocalVarId -> Subst -> BindingForm -> BindingForm
-- Variable in backticks in block or selector (e.g. `X`)
substituteBform _ subst sv@(BSubVar _) = sv

-- If the Style variable is "LOCAL", then resolve it to a unique id for the block and selector
-- Otherwise, look up the substitution for the Style variable and return a Substance variable
substituteBform lv subst sv@(BStyVar sv'@(StyVar' vn)) =
   if vn == localKeyword
   then case lv of
        -- lv = Nothing: substituting into selector, so local vars don't matter
        -- lv = Just (i, j): substituting into block, so local vars matter
        Nothing -> error "LOCAL keyword found without a subst/block id. It should not be used in a selector."
        Just localVarId -> BSubVar $ VarConst $ mkLocalVarName localVarId
   else case M.lookup sv' subst of
        Just subVar -> BSubVar subVar -- Returns result of mapping if it exists (y -> x)
        Nothing     -> sv -- error $ "No subst found for Sty var '" ++ vn ++ "'"
                       -- TODO: no substitutions for namespaces

substituteExpr :: Subst -> SelExpr -> SelExpr
-- theta(B) = ...
substituteExpr subst (SEBind bvar) = SEBind $ substituteBform Nothing subst bvar
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
substituteRel subst (RelBind bvar sExpr) = RelBind (substituteBform Nothing subst bvar)
                                                   (substituteExpr subst sExpr)
-- theta(Q([a]) = Q([theta(a)])
substituteRel subst (RelPred pred) = RelPred $ substitutePred subst pred

-- Applies a substitution to a list of relational statement theta([|S_r])
-- TODO: assumes a full substitution
substituteRels :: Subst -> [RelationPattern] -> [RelationPattern]
substituteRels subst rels = map (substituteRel subst) rels

----- Substs for the translation semantics (more tree-walking on blocks, just changing binding forms)

substitutePath :: LocalVarId -> Subst -> Path -> Path
substitutePath lv subst path =
    case path of
    FieldPath    bVar field      -> FieldPath    (substituteBform (Just lv) subst bVar) field
    PropertyPath bVar field prop -> PropertyPath (substituteBform (Just lv) subst bVar) field prop

substituteField :: LocalVarId -> Subst -> PropertyDecl -> PropertyDecl
substituteField lv subst (PropertyDecl field expr) = PropertyDecl field $ substituteBlockExpr lv subst expr


-- DEPRECATED
-- substituteLayering :: LocalVarId -> Subst -> LExpr -> LExpr
-- substituteLayering lv subst (LId bVar) = LId $ substituteBform (Just lv) subst bVar
-- substituteLayering lv subst (LPath path) = LPath $ substitutePath lv subst path
-- substituteLayering lv subst (LayeringOp op lex1 lex2) =
--                    LayeringOp op (substituteLayering lv subst lex1) (substituteLayering lv subst lex1)

substituteBlockExpr :: LocalVarId -> Subst -> Expr -> Expr
substituteBlockExpr lv subst expr =
    case expr of
    EPath path        -> EPath $ substitutePath lv subst path
    CompApp f es      -> CompApp f $ map (substituteBlockExpr lv subst) es
    ObjFn   f es      -> ObjFn   f $ map (substituteBlockExpr lv subst) es
    ConstrFn  f es    -> ConstrFn f $ map (substituteBlockExpr lv subst) es
    AvoidFn   f es    -> AvoidFn  f $ map (substituteBlockExpr lv subst) es
    BinOp op e1 e2    -> BinOp op (substituteBlockExpr lv subst e1) (substituteBlockExpr lv subst e2)
    UOp   op e        -> UOp   op (substituteBlockExpr lv subst e)
    List es           -> List $ map (substituteBlockExpr lv subst) es
    ListAccess path i -> ListAccess (substitutePath lv subst path) i
    Ctor gpi fields   -> Ctor gpi $ map (substituteField lv subst) fields
    Layering path1 path2 -> Layering (substitutePath lv subst path1) (substitutePath lv subst path2)
    -- No substitution for literals
    IntLit _          -> expr
    AFloat _          -> expr
    StringLit _       -> expr
    BoolLit _         -> expr
    -- TODO: check if this is right
    PluginAccess pluginName e1 e2 -> PluginAccess pluginName (substituteBlockExpr lv subst e1) (substituteBlockExpr lv subst e2)
    Tuple e1 e2 -> Tuple (substituteBlockExpr lv subst e1) (substituteBlockExpr lv subst e2)
    ThenOp e1 e2 -> ThenOp (substituteBlockExpr lv subst e1) (substituteBlockExpr lv subst e2)

substituteLine :: LocalVarId -> Subst -> Stmt -> Stmt
substituteLine lv subst line =
    case line of
    Assign   path expr -> Assign (substitutePath lv subst path) (substituteBlockExpr lv subst expr)
    Override path expr -> Override (substitutePath lv subst path) (substituteBlockExpr lv subst expr)
    Delete   path      -> Delete $ substitutePath lv subst path

-- Assumes a full substitution
substituteBlock :: (Subst, Int) -> (Block, Int) -> Block
substituteBlock (subst, substNum) (block, blockNum) = map (substituteLine (blockNum, substNum) subst) block

----- Filter with relational statements

-- Intentionally pulling out variable equality judgment (. |- x1 = x2) as string equality
varsEq :: Var -> Var -> Bool
varsEq = (==)

exprToVar :: C.Expr -> Var
exprToVar (C.VarE v) = v
exprToVar e = error $ "Style expression matching does not yet handle nested expressions: '" ++ show e ++ "'"

findType :: VarEnv -> String -> [T]
findType typeEnv name =
         case M.lookup name (valConstructors typeEnv) of
         Just vc -> tlsvc vc ++ [tvc vc]
         Nothing -> error $ "name '" ++ name ++ "' does not exist in Substance val constructor environment"
         -- shouldn't happen, since the Sub/Sty should have been statically checked)

exprsMatchArr :: VarEnv -> C.Func -> C.Func -> Bool
exprsMatchArr typeEnv subE styE =
           let subArrType = findType typeEnv $ C.nameFunc subE
               styArrType = findType typeEnv $ C.nameFunc styE
               subVarArgs = map exprToVar $ C.argFunc subE
               styVarArgs = map exprToVar $ C.argFunc styE
           in let res = isSubtypeArrow subArrType styArrType typeEnv
                        && (all (uncurry varsEq) $ zip subVarArgs styVarArgs) in
              trM1 ("subArrType: " ++ show subArrType
                   ++ "\nstyArrType: " ++ show styArrType
                   ++ "\nres: " ++ show (isSubtypeArrow subArrType styArrType typeEnv)) res

-- New judgment (COMBAK number): expression matching that accounts for subtyping. G, B, . |- E0 <| E1
-- We assume the latter expression has already had a substitution applied
exprsMatch :: VarEnv -> C.Expr -> C.Expr -> Bool
-- rule Match-Expr-Var
exprsMatch typeEnv (C.VarE subVar) (C.VarE styVar) = varsEq subVar styVar

-- Match value constructor applications if one val ctor is a subtype of another
-- whereas for function applications, we match only if the exprs are equal (for now)
-- This is because a val ctor doesn't "do" anything besides wrap its values
-- whereas functions with the same type could do very different things, so we don't
-- necessarily want to match them by subtyping
-- (e.g. think of the infinite functions from Vector -> Vector)
-- rule Match-Expr-Vconsapp
exprsMatch typeEnv (C.ApplyValCons subE) (C.ApplyValCons styE) =
           exprsMatchArr typeEnv subE styE
-- rule Match-Expr-Fnapp
exprsMatch typeEnv (C.ApplyFunc subE) (C.ApplyFunc styE) =
           subE == styE
exprsMatch _ _ _ = False

-- Judgment 11. b; theta |- S <| |S_r
relMatchesLine :: VarEnv -> C.SubEnv -> C.SubStmt -> RelationPattern -> Bool
-- rule Bind-Match
relMatchesLine typeEnv subEnv s1@(C.Bind var expr) s2@(RelBind bvar sExpr) =
               case bvar of
               BStyVar v -> error ("Style variable '" ++ show v ++ "' found in relational statement '" ++ show (RelBind bvar sExpr) ++ "'. Should not be present!")
               BSubVar sVar ->
                       let selExpr = toSubExpr sExpr in
                       let res = (varsEq var sVar && exprsMatch typeEnv expr selExpr)
                                 || C.exprsDeclaredEqual subEnv expr selExpr -- B |- E = |E
                       in trM1 ("Bind-Match: trying to match exprs \n'" ++ show s1 ++ "'\n'" ++ show s2 ++ "'\n\n") $ res

-- rule Pred-Match
relMatchesLine typeEnv subEnv s1@(C.ApplyP pred) s2@(RelPred sPred) =
               let selPred = toSubPred sPred
                   res = pred == selPred -- self-equal
                         || C.predsDeclaredEqual subEnv pred selPred -- B |- Q <-> |Q
               in trM1 ("Pred-Match: trying to match exprs \n'" ++ show pred ++ "'\n'" ++ show selPred ++ "'\n'" ++ "res: " ++ show res ++ "'\n\n") $ res
relMatchesLine _ _ _ _ = False -- no other line forms match each other (decl, equality, etc.)

-- Judgment 13. b |- [S] <| |S_r
relMatchesProg :: VarEnv -> C.SubEnv -> C.SubProg -> RelationPattern -> Bool
relMatchesProg typeEnv subEnv subProg rel = any (flip (relMatchesLine typeEnv subEnv) rel) subProg

-- Judgment 15. b |- [S] <| [|S_r]
allRelsMatch :: VarEnv -> C.SubEnv -> C.SubProg -> [RelationPattern] -> Bool
allRelsMatch typeEnv subEnv subProg rels = all (relMatchesProg typeEnv subEnv subProg) rels

-- Optimization to filter out Substance statements that have no hope of matching any of the substituted relation patterns, so we don't do redundant work for every substitution (of which there could be millions). This function is only called once per selector.
couldMatchRels :: VarEnv -> [RelationPattern] -> C.SubStmt -> Bool
couldMatchRels typeEnv rels stmt = any (flip couldMatchARel stmt) rels
  where couldMatchARel :: RelationPattern -> C.SubStmt -> Bool
        -- Heuristic: check if names are equal
        couldMatchARel (RelBind _ (SEAppFunc styF _)) (C.Bind _ (C.ApplyFunc subF)) =
                       styF == C.nameFunc subF

        couldMatchARel (RelBind _ (SEAppValCons styVC _)) (C.Bind _ (C.ApplyValCons subVC')) =
                       let subVC = C.nameFunc subVC' in
                       styVC == subVC
                       || isSubtypeArrowNamed typeEnv subVC styVC -- check value ctor subtyping

        couldMatchARel (RelPred styPred) (C.ApplyP subPred) =
                       predicateName styPred == (nameOf $ C.predicateName subPred)

        couldMatchARel _ _ = False

        nameOf (C.PredicateConst n) = n

isSubtypeArrowNamed :: VarEnv -> String -> String -> Bool
isSubtypeArrowNamed typeEnv subCtor styCtor =
            let subArrType = findType typeEnv subCtor
                styArrType = findType typeEnv styCtor
            in isSubtypeArrow subArrType styArrType typeEnv

-- Judgment 17. b; [theta] |- [S] <| [|S_r] ~> [theta']
-- Folds over [theta]
filterRels :: VarEnv -> C.SubEnv -> C.SubProg -> [RelationPattern] -> [Subst] -> [Subst]
filterRels typeEnv subEnv subProg rels substs =
           let subProgFiltered = filter (couldMatchRels typeEnv rels) subProg in
           -- trace ("\n----------\n\nrels:\n" ++ ppShow rels
           --        ++ "\nsub prog filtered\n: " ++ ppShow subProgFiltered) $
           filter (\subst -> allRelsMatch typeEnv subEnv subProgFiltered 
                             (substituteRels subst rels)) substs

----- Match declaration statements

-- Judgment 9. G; theta |- T <| |T
-- Assumes types are nullary, so doesn't return a subst, only a bool indicating whether the types matched
matchType :: VarEnv -> T -> StyT -> Bool
matchType varEnv substanceType@(TConstr tctor) styleType@(STCtor stctor) =
          if not (null (argCons tctor)) || not (null (argConsS stctor))
          then error "no types with parameters allowed in match" -- TODO error msg
          else trM1 ("types: " ++ nameCons tctor ++ ", " ++ nameConsS stctor) $
               nameCons tctor == nameConsS stctor
               -- Only match if a Substance type is a subtype of a Style type (e.g. Interval matches ClosedInterval)
               || isSubtype substanceType (toSubType styleType) varEnv
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
              else trM1 "types didn't match" Nothing -- substitution is only valid if types matched first
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

find_substs_sel :: VarEnv -> C.SubEnv -> C.SubProg -> (Header, SelEnv) -> [Subst]
-- Judgment 19. g; G; b; [theta] |- [S] <| Sel
-- NOTE: this uses little gamma (not in paper) to check substitution validity
find_substs_sel varEnv subEnv subProg (Select sel, selEnv) =
    let decls            = selHead sel ++ selWith sel
        rels             = selWhere sel
        initSubsts       = []
        rawSubsts        = matchDecls varEnv subProg decls initSubsts
        subst_candidates = filter (fullSubst selEnv)
                           $ trace ("rawSubsts: # " ++ show (length rawSubsts))
                           rawSubsts
        -- TODO: check validity of subst_candidates (all StyVars have exactly one SubVar)
        filtered_substs  = trM1 ("candidates: " ++ show subst_candidates) $
                           filterRels varEnv subEnv subProg rels subst_candidates
        correct_substs   = filter uniqueKeysAndVals filtered_substs
    in correct_substs
find_substs_sel _ _ _ (Namespace _, _) = [] -- No substitutions for a namespace (not in paper)

-- TODO: add note on prog, header judgment to paper?
-- Find a list of substitutions for each selector in the Sty program.
find_substs_prog :: VarEnv -> C.SubEnv -> C.SubProg -> StyProg -> [SelEnv] -> [[Subst]]
find_substs_prog varEnv subEnv subProg styProg selEnvs =
    let sels = map fst styProg in
    let selsWithEnvs = zip sels selEnvs in
    map (find_substs_sel varEnv subEnv subProg) selsWithEnvs

-- TODO: make anonymous variables for unification for Substance program

-------------------- Translation dynamics (i.e. actual Style compiler)

-- TODO: block statics
checkBlock :: SelEnv -> Block -> [Error]
checkBlock selEnv block = []

----- Type definitions
type GPICtor = String

data TagExpr a = OptEval Expr      -- ^ Thunk evaluated at each step of optimization-time
               | Done (Value a)    -- ^ A value in the host language, fully evaluated
               | Pending (Value a) -- ^ A value to be updated afterwards (e.g. label dimensions)
    deriving (Show, Eq, Typeable)

type PropertyDict a = M.Map Property (TagExpr a)
type FieldDict a = M.Map Field (FieldExpr a)

data FieldExpr a = FExpr (TagExpr a)
                 | FGPI ShapeTypeStr (PropertyDict a)
    deriving (Show, Eq, Typeable)

type Warning = String
data Name = Sub String   -- Sub obj name
            | Gen String -- randomly generated name
    deriving (Show, Eq, Ord, Typeable)

data Translation a = Trans { trMap    :: M.Map Name (FieldDict a),
                             warnings :: [Warning] }
    deriving (Show, Eq, Typeable)

type OverrideFlag = Bool

-- For a Substance object "A", the Translation might look like this:
-- Trans [ "A" =>
--        FieldDict [ "val"   => FExpr (Done (IntLit 2)),
--                    "shape" => GPI Circ PropMap [ "r" => OptEval (EPath (FieldPath (SubVar (VC "B"))) "val"),
--                                                  "x" => ...  ] ] ]

----- Translation util functions

initTrans :: forall a . Autofloat a => Translation a
initTrans = Trans { trMap = M.empty, warnings = [] }

-- Convert Sub bvar name to Sub name in Translation
trName :: BindingForm -> Name
trName (BSubVar (VarConst nm)) = Sub nm
trName (BStyVar (StyVar' nm))  = Sub nm
       -- error ("Style variable '" ++ show bv ++ "' in block! Was a non-full substitution applied?") -- TODO/URGENT fix for namespaces

nameStr :: Name -> String
nameStr (Sub s) = s
nameStr (Gen s) = s

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

-- TODO clean these up
pathStr :: Path -> String
pathStr (FieldPath bvar field) = intercalate "." [show bvar, field]
pathStr (PropertyPath bvar field property) = intercalate "." [show bvar, field, property]

pathStr2 :: Name -> Field -> String
pathStr2 name field = intercalate "." [nameStr name, field]

pathStr3 :: Name -> Field -> Property -> String
pathStr3 name field property = intercalate "." [nameStr name, field, property]

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
        path = pathStr3 name field property in
    case M.lookup name trn of
    Nothing ->
        let err = "Err: Sub obj '" ++ nameStr name ++ "' has no fields; can't delete path '" ++ path ++ "'" in
        addWarn trans err
    Just fieldDict ->
        case M.lookup field fieldDict of
        Nothing -> let err = "Err: Sub obj '" ++ nameStr name ++ "' already lacks field '" ++ field
                              ++ "'; can't delete path " ++ path in
                   addWarn trans err
        -- Deal with path aliasing as in `addProperty`
        Just (FExpr e) ->
             case e of
             OptEval (EPath p@(FieldPath bvar newField)) ->
                            let newName = trName bvar in
                            if newName == name && newField == field
                            then let err = "Error: path '" ++ pathStr p ++ "' was aliased to itself"
                                 in addWarn trans err
                            else deleteProperty trans newName newField property
             res -> let err = "Error: Sub obj '" ++ nameStr name ++ "' does not have GPI '"
                              ++ field ++ "'; cannot delete property '" ++ property ++ "'" in
                    addWarn trans err
        Just (FGPI ctor properties) ->
           -- If the field is GPI, check if property already exists
           if property `M.notMember` properties
           then let warn = "Warning: property '" ++ property ++ "' already does not exist in path '"
                           ++ pathStr3 name field property ++ "'; deletion does nothing"
                in addWarn trans warn
           else let properties' = M.delete property properties
                    fieldDict'  = M.insert field (FGPI ctor properties') fieldDict
                    trn'        = M.insert name fieldDict' trn in
                trans { trMap = trn' }

-- Implements two rules for fields:
-- x.n = Ctor { n_i = e_i }, rule Line-Set-Ctor, for GPI
-- x.n = e, rule Line-Set-Field-Expr
addField :: (Autofloat a) => OverrideFlag -> Translation a ->
                             Name -> Field -> TagExpr a -> Translation a
addField override trans name field texpr =
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
    let fieldExpr = case texpr of
                    OptEval (Ctor ctorName propertyDecls) -> -- rule Line-Set-Ctor
                         FGPI ctorName (mkPropertyDict propertyDecls)
                    _ -> FExpr texpr in   -- rule Line-Set-Field-Expr
    let fieldDict' = M.insert field fieldExpr fieldDict
        trn' = M.insert name fieldDict' trn in
    trans { trMap = trn', warnings = addMaybes (warnings trans) [warn1, warn2, warn3] }

addProperty :: (Autofloat a) => OverrideFlag -> Translation a ->
                                Name -> Field -> Property -> TagExpr a -> Translation a
addProperty override trans name field property texpr =
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
        -- If looking up "f.domain" yields a *different* path (i.e. that path was an alias)
        -- e.g. "f.domain" is aliased to "I.shape"
        -- then call addProperty with the other path, otherwise fail
        Just (FExpr e) ->
             case e of
             OptEval (EPath p@(FieldPath bvar newField)) ->
                            let newName = trName bvar in
                            if newName == name && newField == field
                            then let err = "Error: path '" ++ pathStr p ++ "' was aliased to itself"
                                 in addWarn trans err
                            else addProperty override trans newName newField property texpr
             res -> let err = "Error: Sub obj '" ++ nameStr name ++ "' does not have GPI '"
                              ++ field ++ "'; found expr '" ++ show res ++ "'; cannot add property '" ++ property ++ "'" in
                    addWarn trans err
        Just (FGPI ctor properties) ->
           -- If the field is GPI, check if property already exists and whether it matches the override setting
           let warn = if (property `M.notMember` properties) && override
                      then Just $ "Warning: property '" ++ property ++ "' does not exist in path '"
                           ++ pathStr3 name field property ++ "' but override was set"
                      else if property `M.member` properties && (not override)
                      then Just $ "Warning: property '" ++ property ++ "' already exists in path '"
                           ++ pathStr3 name field property ++ "' but override was not set"
                      else Nothing in
           let properties' = M.insert property texpr properties
               fieldDict'  = M.insert field (FGPI ctor properties') fieldDict
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

addPath :: (Autofloat a) => OverrideFlag -> Translation a -> Path -> TagExpr a -> Either [Error] (Translation a)
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

addPaths :: (Autofloat a) => OverrideFlag -> Translation a -> [(Path, TagExpr a)] -> Either [Error] (Translation a)
addPaths override = foldM (\trans (p, e) -> addPath override trans p e) 

----- Translation judgments

{- Note: All of the folds below use foldM.
   foldM stops accumulating when the first fatal error is reached, using "Either [Error]" as a monad
   (Non-fatal errors are stored as warnings in the translation)
   foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
   example:
   f acc elem = if elem < 0 then Left ["wrong " ++ show elem] else Right $ elem : acc
   foldM f [] [1, 9, -1, 2, -2] = Left ["wrong -1"]
   foldM f [] [1, 9] = Right [9,1]  -}

-- Judgment 26. D |- phi ~> D'
-- This is where interesting things actually happen (each line is interpreted and added to the translation)
translateLine :: (Autofloat a) => Translation a -> Stmt -> Either [Error] (Translation a)
translateLine trans stmt =
    case stmt of
    Assign path expr   -> addPath False trans path (OptEval expr)
    Override path expr -> addPath True trans path (OptEval expr)
    Delete path        -> deletePath trans path

-- Judgment 25. D |- |B ~> D' (modified to be: theta; D |- |B ~> D')
translateBlock :: (Autofloat a) => (Block, Int) -> Translation a -> (Subst, Int) ->
                                                               Either [Error] (Translation a)
translateBlock blockWithNum trans substNum =
    let block' = substituteBlock substNum blockWithNum in
    foldM translateLine trans block'

-- Judgment 24. [theta]; D |- |B ~> D'
translateSubstsBlock :: (Autofloat a) => Translation a -> [(Subst, Int)] ->
                                                     (Block, Int) -> Either [Error] (Translation a)
translateSubstsBlock trans substsNum blockWithNum = foldM (translateBlock blockWithNum) trans substsNum

-- Judgment 23, contd.
translatePair :: (Autofloat a) => VarEnv -> C.SubEnv -> C.SubProg ->
                                  Translation a -> ((Header, Block), Int) -> Either [Error] (Translation a)
translatePair varEnv subEnv subProg trans ((Namespace styVar, block), blockNum) =
    let selEnv = initSelEnv
        bErrs  = checkBlock selEnv block in
    if null (sErrors selEnv) && null bErrs
        then let subst = M.empty in -- is this the correct empty?
             translateBlock (block, blockNum) trans (subst, 0) -- skip transSubstsBlock; only one subst
        else Left $ sErrors selEnv ++ bErrs

translatePair varEnv subEnv subProg trans ((header@(Select sel), block), blockNum) =
    let selEnv = checkSel varEnv sel
        bErrs  = checkBlock selEnv block in
    if null (sErrors selEnv) && null bErrs
        then let substs = find_substs_sel varEnv subEnv subProg (header, selEnv) in
             let numberedSubsts = zip substs [0..] in -- For creating unique local var names
             translateSubstsBlock trans numberedSubsts (block, blockNum)
        else Left $ sErrors selEnv ++ bErrs

insertLabels :: (Autofloat a, Eq a) => Translation a -> C.LabelMap -> Translation a
insertLabels trans labels =
    trans { trMap = M.mapWithKey insertLabel (trMap trans),
            warnings = warnings trans ++ ["Note: Text GPIs are automatically deleted if their Substance object has no label"]
            -- TODO: print out the names of the GPIs that were auto-deleted
          }
    where
        toFieldStr :: Eq a => String -> FieldExpr a
        toFieldStr s = FExpr $ Done $ StrV s

        insertLabel :: Eq a => Name -> FieldDict a -> FieldDict a
        insertLabel (Sub s) fieldDict = 
            let labelField = "label" in
            case M.lookup s labels of
                Nothing ->  fieldDict
                -- NOTE: maybe this is a "namespace," so we pass through
                Just (Just l) -> M.insert labelField (toFieldStr l) fieldDict
                                 -- If "NoLabel", default to an empty string *and* delete any Text GPIs that use it
                Just Nothing  -> let fd' = M.insert labelField (toFieldStr "") fieldDict in
                                 M.filter (not . usesLabelText s) fd'
        -- only insert labels for Substance objects
        insertLabel (Gen _) fieldDict = fieldDict

        -- Return True if it's a Text GPI that uses the label
        -- TODO: should probably do something with other GPIs/fields that use the label (delete/filter/modify them?)
        -- as well as recursively delete anything else that refers to *those* things
        usesLabelText :: Eq a => String -> FieldExpr a -> Bool
        usesLabelText subName (FExpr _) = False
        usesLabelText subName (FGPI shapeType properties) =
                      let labelForm = OptEval (EPath (FieldPath (BSubVar (VarConst subName)) "label"))
                          usesLabel = case M.lookup "string" properties of
                                      Nothing -> False
                                      Just expr -> expr == labelForm
                      in shapeType == "Text" && usesLabel

-- For any Substance object (say one called "K1"), insert the field mapping `K1.name = "K1"` which will be accessible from Style.
-- This is generally for plugin accessors to use in Style, so they can access the right field of the Style values returned by a plugin.
insertNames :: (Autofloat a, Eq a) => Translation a -> Translation a
insertNames trans = trans { trMap = M.mapWithKey insertName $ trMap trans }
            where insertName :: Name -> FieldDict a -> FieldDict a -- I guess we don't need to insert names for namespaces
                  insertName (Sub name) fieldDict = 
                             let nameField = "name" in
                             M.insert nameField (FExpr $ Done $ StrV name) fieldDict
                  insertName (Gen _) fieldDict = fieldDict
                          
{- Find plugin accessor expressions
Evaluates a plugin accessor’s subexpressions (the primary and secondary key)
  Static evaluate: look up value in translation (perform compile-time computations)
  Check their types (both need to be strings)
Look up the result in the plugin’s JSON output
Store it in the translation in place of the accessor, as a finished float -}
     
evalPluginAccess :: (Autofloat a) => StyValMap a -> Translation a -> Translation a
evalPluginAccess valMap trans = 
                 trans { trMap = M.mapWithKey (evalFieldAccesses valMap) $ trMap trans }

            where evalFieldAccesses :: (Autofloat a) => StyValMap a -> Name -> FieldDict a -> FieldDict a
                  evalFieldAccesses vmap (Sub name) fieldDict = 
                       M.mapWithKey (evalFieldAccess vmap) fieldDict
                  evalFieldAccesses vmap (Gen _) fieldDict = fieldDict

                  evalFieldAccess :: (Autofloat a) => StyValMap a -> Field -> FieldExpr a -> FieldExpr a
                  evalFieldAccess vmap field (FExpr te) = FExpr $ evalTExpr vmap te
                  evalFieldAccess vmap field (FGPI stype properties) =
                      let properties' = M.mapWithKey (evalPropertyAccess vmap) properties in
                      FGPI stype properties'
                  
                  evalPropertyAccess :: (Autofloat a) => StyValMap a -> Property -> TagExpr a -> TagExpr a
                  evalPropertyAccess vmap property te = evalTExpr vmap te

                  evalTExpr :: (Autofloat a) => StyValMap a -> TagExpr a -> TagExpr a
                  evalTExpr vmap val@(Done _) = val
                  evalTExpr vmap (OptEval e) = OptEval $ evalPluginExpr vmap e
                  -- TODO: check if the result was a string, and if so, make it a done value?

                  evalPluginExpr :: (Autofloat a) => StyValMap a -> Expr -> Expr
                  -- Do work: evaluate something like ddg[A.name]["x"] by evaluating each key and looking up the result in JSON
                  evalPluginExpr vmap (PluginAccess p e1 e2) = 
                      -- TODO: actually check/use the plugin string; this currently ignores the plugin `p`
                      -- just use the trans in the context of `evalPluginAccess` for now, TODO: fix to pass it around?
                      -- or remove the vmap from the context
                      let e1' = evalStatic e1 
                          e2' = evalStatic e2
                          res = lookupStyVal e1' e2' vmap
                      in AFloat $ Fix $ r2f res

                  -- Look for work to do (might involve strings)
                  evalPluginExpr vmap (CompApp s es) = CompApp s $ map (evalPluginExpr vmap) es
                  evalPluginExpr vmap (ObjFn s es) = ObjFn s $ map (evalPluginExpr vmap) es
                  evalPluginExpr vmap (ConstrFn s es) = ConstrFn s $ map (evalPluginExpr vmap) es
                  evalPluginExpr vmap (AvoidFn s es) = AvoidFn s $ map (evalPluginExpr vmap) es
                  evalPluginExpr vmap (BinOp o e1 e2) = BinOp o (evalPluginExpr vmap e1) (evalPluginExpr vmap e2)
                  evalPluginExpr vmap (UOp o e) = UOp o $ evalPluginExpr vmap e
                  evalPluginExpr vmap (List es) = List $ map (evalPluginExpr vmap) es
                  evalPluginExpr vmap (Tuple e1 e2) = Tuple (evalPluginExpr vmap e1) (evalPluginExpr vmap e2)
                  evalPluginExpr vmap (ThenOp e1 e2) = ThenOp (evalPluginExpr vmap e1) (evalPluginExpr vmap e2)

                  -- Leaves (no strings should be involved)
                  evalPluginExpr _ e@(IntLit _) = e
                  evalPluginExpr _ e@(AFloat _) = e
                  evalPluginExpr _ e@(StringLit _) = e
                  evalPluginExpr _ e@(BoolLit _) = e
                  evalPluginExpr _ e@(EPath _) = e
                  evalPluginExpr _ e@(ListAccess _ _) = e
                  evalPluginExpr _ e@(Ctor _ _) = e
                  evalPluginExpr _ e@(Layering _ _) = e

                  evalStatic :: Expr -> String
                  evalStatic (StringLit s) = s
                  evalStatic (EPath (FieldPath bvar field)) = 
                    case lookupField bvar field trans of -- TODO: clean up lookupField location
                    FExpr (OptEval (StringLit s)) -> s
                    FExpr (Done (StrV s)) -> s
                    _ -> error "expected string for plugin accessor key; after looking up field, didn't get string"
                  evalStatic (EPath (PropertyPath (BSubVar v) field property)) = error "plugin TODO"
                  evalStatic e = error "expected string or path expression for plugin accessor key; did not get a string"

-- TODO: add beta in paper and to comment below
-- Judgment 23. G; D |- [P]; |P ~> D'
-- Fold over the pairs in the Sty program, then the substitutions for a selector, then the lines in a block.
translateStyProg :: forall a . (Autofloat a) =>
    VarEnv -> C.SubEnv -> C.SubProg -> StyProg -> C.LabelMap -> [J.StyVal] ->
    Either [Error] (Translation a)
translateStyProg varEnv subEnv subProg styProg labelMap styVals =
    let numberedProg = zip styProg [0..] in -- For creating unique local var names
    case foldM (translatePair varEnv subEnv subProg) initTrans numberedProg of
        Right trans -> 
              let transWithNames = insertNames trans
                  transWithNamesAndLabels = insertLabels transWithNames labelMap 
                  styValMap = styJsonToMap styVals
                  transWithPlugins = evalPluginAccess styValMap transWithNamesAndLabels
              in trace "translateStyProg: " $ Right transWithPlugins
        Left errors -> Left errors

---------- Plugin accessors

type SubObjName = String
type PropertyName = String
type StyValMap a = M.Map SubObjName (M.Map PropertyName a)

styJsonToMap :: (Autofloat a) => [J.StyVal] -> StyValMap a
styJsonToMap vals = 
             M.fromList (map (\v -> (subName v, 
                                     M.fromList (map (\w -> (propertyName w, 
                                                             r2f $ propertyVal w)) 
                                                    (nameVals v)))) 
                             vals)

-- TODO move lookups to utils; this was moved from GenOptProblem
lookupField :: (Autofloat a) => BindingForm -> Field -> Translation a -> FieldExpr a
lookupField bvar field trans =
    let name = trName bvar in
    let trn = trMap trans in
    case M.lookup name trn of
    Nothing -> error ("path '" ++ pathStr2 name field ++ "''s name doesn't exist in trans")
               -- TODO improve error messages and return error messages (Either [Error] (TagExpr a))
    Just fieldDict ->
         case M.lookup field fieldDict of
         Nothing -> error ("path '" ++ pathStr2 name field ++ "'s field doesn't exist in trans")
         Just fexpr -> fexpr
              -- TODO: This is the right way to look up fields, but doing so causes a frontend undefined error. Why?

              -- case fexpr of 
              -- -- Deal with field aliases, e.g. `f.codomain = R.shape`. Keep looking up paths until we get a GPI or expression.
              -- FExpr (OptEval (EPath (FieldPath bvarSynonym fieldSynonym))) ->
              --   if bvar == bvarSynonym && field == fieldSynonym
              --   then error ("nontermination in lookupField with path '" ++ pathStr2 name field ++ "' set to itself")
              --   else trace ("Recursively looking up field " ++ pathStr (FieldPath bvar field) ++ " -> " ++ pathStr (FieldPath bvarSynonym fieldSynonym)) lookupField bvarSynonym fieldSynonym trans
              -- _ -> fexpr

shapeType :: (Autofloat a) => BindingForm -> Field -> Translation a -> ShapeTypeStr
shapeType bvar field trans =
          case lookupField bvar field trans of
          FGPI stype _ -> stype
              -- -- Deal with field aliases, e.g. `f.codomain = R.shape`. Keep looking up paths until we get a GPI or expression.
          FExpr (OptEval (EPath (FieldPath bvarSynonym fieldSynonym))) ->
            if bvar == bvarSynonym && field == fieldSynonym
            then error ("nontermination in lookupField with path '" ++ pathStr (FieldPath bvar field) ++ "' set to itself")
            else {- trace ("Recursively looking up field " ++ pathStr (FieldPath bvar field) ++ " -> " ++ pathStr (FieldPath bvarSynonym fieldSynonym)) -} shapeType bvarSynonym fieldSynonym trans

          FExpr e -> error ("path " ++ show e ++ " is not a GPI; cannot get type")

lookupStyVal :: (Autofloat a) => String -> String -> StyValMap a -> a
lookupStyVal subName propName vmap =
    case M.lookup subName vmap of
    Nothing -> error ("plugin accessors in style: for access " ++ toAccess subName propName ++ ", primary key does not exist")
    Just propDict ->
         case M.lookup propName propDict of
         Nothing -> error ("plugin accessors in style: for access " ++ toAccess subName propName ++ ", property does not exist")
         Just val -> val
    where toAccess s1 s2 = s1 ++ "." ++ s2

--------------------------------------------------------------------------------
-- Debugging

-- main :: IO ()
-- main = do
--     let f = "sty/test.sty"
--
--     let elementFile = "real-analysis-domain/real-analysis.dsl"
--     elementIn <- readFile elementFile
--     elementEnv <- parseElement elementFile elementIn
--
--     prog <- readFile f
--     res <- parseInstantiation f prog elementEnv
--     print res
