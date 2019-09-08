{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_HADDOCK prune #-}

module Penrose.SubstanceJSON where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.List            as L
import           Debug.Trace
import           GHC.Generics

import qualified Penrose.Env          as E
import           Penrose.Substance

-------------------------------------------------------
-- | JSON schemas and derivations using Aeson
data FunctionSchema = FunctionSchema
  { varName   :: String
  , fname     :: String
  , fargNames :: [String]
  } deriving (Generic, Show)

data PredicateSchema = PredicateSchema
  { pname     :: String
  , pargNames :: [String]
  } deriving (Generic, Show)

data ConstraintSchema = ConstraintSchema
  { functions  :: [FunctionSchema]
  , predicates :: [PredicateSchema]
  } deriving (Generic, Show)

data ObjectSchema = ObjectSchema
  { objType :: String
  , objName :: String
  } deriving (Generic, Show)

data SubSchema = SubSchema
  { objects     :: [ObjectSchema]
  , constraints :: ConstraintSchema
  , values      :: [ValueSchema]
  } deriving (Generic, Show)

data ValueSchema = ValueSchema
  { name  :: String
  , value :: String -- TODO: support more formats in the future
  } deriving (Generic, Show)

instance FromJSON ValueSchema

instance ToJSON ValueSchema where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON ObjectSchema where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ObjectSchema

instance ToJSON PredicateSchema where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON PredicateSchema

instance ToJSON FunctionSchema where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON FunctionSchema

instance ToJSON ConstraintSchema where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ConstraintSchema

instance ToJSON SubSchema where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON SubSchema

-------------------------------------------------------
targToSchema :: E.Arg -> String
targToSchema (E.AVar (E.VarConst s)) = s
targToSchema (E.AT t)                = tToSchema t

tToSchema :: E.T -> String
tToSchema (E.TTypeVar typeVar) = E.typeVarName typeVar
tToSchema (E.TConstr typeCtorApp) =
  let ctorString = E.nameCons typeCtorApp
      args = E.argCons typeCtorApp
  in case args of
       [] -> ctorString -- "Set"
       _ ->
         ctorString ++
         "(" ++ (L.intercalate ", " $ map targToSchema args) ++ ")"

exprToSchema :: Expr -> String
exprToSchema (VarE (E.VarConst v)) = v
exprToSchema _ =
  error
    "Cannot convert anonymous expressions in function/val ctor arguments to JSON; must name them first"

prednameToSchema :: PredicateName -> String
prednameToSchema (PredicateConst s) = s

predargToSchema :: PredArg -> String
predargToSchema (PE expr) = exprToSchema expr
predargToSchema (PP pred) =
  error "Cannot convert nested predicate in predicate argument to JSON"

-- | Convert a Substance statement to a JSON format and adds it to the right list
-- | Note: do not rely on ordering in JSON, as this function does not guarantee preserving Substance program order.
-- | However, we do guarantee preserving argument order in function/valcons/predicate applications.
toSchema ::
     ([ObjectSchema], [FunctionSchema], [PredicateSchema], [ValueSchema])
  -> SubStmt
  -> ([ObjectSchema], [FunctionSchema], [PredicateSchema], [ValueSchema])
toSchema acc@(objSchs, fnSchs, predSchs, valSchs) subLine =
  case subLine of
    Decl t (E.VarConst v) ->
      let res = ObjectSchema {objType = tToSchema t, objName = v}
      in (res : objSchs, fnSchs, predSchs, valSchs)
    DeclList t vs ->
      let decls = map (\v -> Decl t v) vs
      in foldl toSchema acc decls
    Bind (E.VarConst v) (ApplyFunc f) ->
      let res =
            FunctionSchema
            { varName = v
            , fname = nameFunc f
            , fargNames = map exprToSchema $ argFunc f
            }
      in (objSchs, res : fnSchs, predSchs, valSchs)
    Bind (E.VarConst v) (ApplyValCons f) ->
      let res =
            FunctionSchema
            { varName = v
            , fname = nameFunc f
            , fargNames = map exprToSchema $ argFunc f
            }
      in (objSchs, res : fnSchs, predSchs, valSchs)
    Bind (E.VarConst v) (StringLit s) ->
      let res = ValueSchema {name = v, value = s}
      in (objSchs, fnSchs, predSchs, res : valSchs)
    ApplyP p ->
      let res =
            PredicateSchema
            { pname = prednameToSchema $ predicateName p
            , pargNames = map predargToSchema $ predicateArgs p
            }
      in (objSchs, fnSchs, res : predSchs, valSchs)
         -- TODO: these forms are not sent to plugins
    Bind _ (VarE _) ->
      trace "WARNING: not sending Substance form to plugin!" acc
    Bind _ (DeconstructorE _) ->
      trace "WARNING: not sending Substance form to plugin!" acc
    EqualE _ _ -> trace "WARNING: not sending Substance form to plugin!" acc
    EqualQ _ _ -> trace "WARNING: not sending Substance form to plugin!" acc
    LabelDecl _ _ -> acc
    AutoLabel _ -> acc
    NoLabel _ -> acc

-- | Turn a Substance prog into the schema format defined above
subToSchema :: SubProg -> SubSchema
subToSchema prog =
  let (objSchs, fnSchs, predSchs, valSchs) =
        foldl toSchema ([], [], [], []) prog
  in SubSchema
     { objects = objSchs
     , constraints =
         ConstraintSchema {functions = fnSchs, predicates = predSchs}
     , values = valSchs
     }

-- | This is the main function for converting a parsed Substance program to JSON format, called in ShadowMain
writeSubstanceToJSON :: FilePath -> SubOut -> IO ()
writeSubstanceToJSON file (SubOut subprog envs labels) = do
  let substanceSchema = subToSchema subprog
  let bytestr = encode substanceSchema
  BL.writeFile file bytestr

---------------------------------------------------------
--- | JSON format for parsing Style values from plugins
data KeyValPair = KeyValPair
  { propertyName :: String
  , propertyVal  :: Float -- TODO: generalize this
  } deriving (Generic, Show)

data StyVal = StyVal
  { subName  :: String
  , nameVals :: [KeyValPair]
  } deriving (Generic, Show)

instance FromJSON KeyValPair

instance FromJSON StyVal

--- Plugin output parsed as [StyVal]
--------------------------------------------------------
-- | Test writing a Substance program in JSON format
main :: IO ()
main
     -- let subProg = "Set A\nSet B\n IsSubset(A,B)\nSet C\nC := Union(A, B)\n\nPoint p\n PointIn(C, p)\nC := AddPoint(p, C)\n\nAutoLabel All"
 = do
  let testFile = "testSubstanceJSON.json"
  let info =
        SubSchema
        { objects = [ObjectSchema {objType = "Set", objName = "A"}]
        , constraints =
            ConstraintSchema
            { functions =
                [ FunctionSchema
                  {varName = "C", fname = "Union", fargNames = ["A", "B"]}
                ]
            , predicates =
                [PredicateSchema {pname = "IsSubset", pargNames = ["C", "p"]}]
            }
        , values = []
        }
  let res = encode info
  BL.writeFile testFile res
