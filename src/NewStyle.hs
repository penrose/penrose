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
import Data.Maybe (fromMaybe)
import Data.List (nubBy, nub, intercalate)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
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
    deriving (Show, Eq, Typeable)

-- | A header of a block is either a selector or a namespace declaration
data Header = Select Selector | NameSpace StyVar
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
          | STConstr STypeConstr
      deriving (Show, Eq, Typeable)

data STypeConstr = STypeConstr {
    nameConsS :: String,
    argConsS  :: [SArg],
    posConsS  :: SourcePos
} deriving (Eq, Typeable)
instance Show STypeConstr where
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
    = PE SelExpr
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
-- Substnace id, which is denoted by the special @``@ characters
data BindingForm = BSubVar Var | BStyVar StyVar
    deriving (Show, Eq, Typeable)

data DeclPattern = PatternDecl' StyT BindingForm
    deriving (Show, Eq, Typeable)

data RelationPattern = RelBind BindingForm SelExpr | RelPred Predicate
    deriving (Show, Eq, Typeable)

-- | A selector is TODO
data Selector = Selector {
    selHead  :: [DeclPattern],
    selWith  :: [DeclPattern],
    selWhere :: [RelationPattern]
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
    Right styProg -> do
        putStrLn "Style AST:\n"
        mapM_ print styProg
        divLine
        return styProg

-- | 'styleParser' is the top-level function that parses a Style proram
styleParser :: Parser StyProg
styleParser = between scn eof styProg

-- | `styProg` parses a Style program, consisting of a collection of one or
-- more blocks
styProg :: Parser StyProg
styProg = some headerBlock
    where headerBlock = (,) <$> header <*> braces block <* scn

header :: Parser Header
header = tryChoice [Select <$> selector, NameSpace <$> styVar]

-------------------- Selector parsers

selector :: Parser Selector
selector = do
    hd       <- declPattern `sepBy1` comma
    (wi, wh) <- scn >> withAndWhere
    return Selector { selHead = hd,  selWith = wi, selWhere = wh }
    where with = option [] (rword "with"  >> declPattern `sepBy1` comma)
          wher = option [] (rword "where" >> relationPattern `sepBy1` comma)
          -- TODO: write it more elegantly
          order1 = do
              wi <- with
              wh <- scn >> wher
              return (wi, wh)
          order2 = do
              wh <- wher
              wi <- scn >> with
              return (wi, wh)
          withAndWhere = order1 <|> order2

styVar :: Parser StyVar
styVar = StyVar' <$> identifier

declPattern :: Parser DeclPattern
declPattern = PatternDecl' <$> styType <*> bindingForm

styType :: Parser StyT
styType = STTypeVar <$> typeVar <|> STConstr <$> typeConstructor

-- COMBAK: what is the syntax of this thing? In Env this is `'E`
typeVar :: Parser STypeVar
typeVar = do
    t   <- identifier
    pos <- getPosition
    return STypeVar' { typeVarNameS = t, typeVarPosS = pos}

typeConstructor :: Parser STypeConstr
typeConstructor = do
    n    <- identifier
    args <- parens (styArgument `sepBy1` comma) -- COMBAK: empty parens?
    pos  <- getPosition
    return STypeConstr { nameConsS = n, argConsS = args, posConsS = pos }

-- COMBAK: styType will probably not work because of recursion. Test this explicitly
styArgument :: Parser SArg
styArgument = SAVar <$> bindingForm <|> SAT <$> styType

relationPattern :: Parser RelationPattern
relationPattern = try pre <|> bind
    where bind = RelBind <$> bindingForm <*> (eq >> selectorExpr)
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
    SEBind       <$> bindingForm <|>
    -- COMBAK: what is the syntactic difference btw the two?
    -- COMBAK: right recursion, empty parens
    SEAppValCons <$> identifier <*> parens (selectorExpr `sepBy1` comma) <|>
    SEAppFunc    <$> identifier <*> parens (selectorExpr `sepBy1` comma)

bindingForm :: Parser BindingForm
bindingForm = BSubVar <$> backticks varParser <|> BStyVar <$> styVar

-------------------- Block parsers

block :: Parser [Stmt]
block = stmt `sepEndBy` newline'

stmt :: Parser Stmt
stmt = assign <|> override <|> delete
-- stmt = return $ Delete (IntLit 1) -- DEBUG: COMBAK and delete

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
           stringLit,
           -- COMBAK: change NewStyle to Style
           makeExprParser term NewStyle.operators
       ]
    -- return $ IntLit 1


term :: Parser Expr
term = parens term
    <|> EPath  <$> path
    <|> try (AFloat <$> annotatedFloat)
    <|> IntLit <$> integer

-- COMBAK: get operators done
operators :: [[Text.Megaparsec.Expr.Operator Parser Expr]]
operators =
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

path :: Parser Path
path = FieldPath    <$> bindingForm <*> dotId <|>
       PropertyPath <$> bindingForm <*> dotId <*> dotId
       where dotId = dot >> identifier

compFn, objFn, constrFn :: Parser Expr
compFn   = CompApp <$> identifier <*> exprsInParens
objFn    = ObjFn <$> (rword "encourage" >> identifier) <*> exprsInParens
constrFn = ConstrFn <$> (rword "ensure" >> identifier) <*> exprsInParens
exprsInParens = parens $ expr `sepBy` comma

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
annotatedFloat = (rword "optimized" >> return Vary) <|> Fix <$> float
