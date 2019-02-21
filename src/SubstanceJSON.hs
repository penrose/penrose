{-# LANGUAGE DeriveGeneric #-}

module SubstanceJSON where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as BL

import Substance

-------------------------------------------------------
-- | JSON schemas and derivations using Aeson

data FunctionSchema = FunctionSchema {
     varName :: String,
     fname :: String,
     fargNames :: [String]
} deriving (Generic, Show)

data PredicateSchema = PredicateSchema {
     pname :: String,
     pargNames :: [String]
} deriving (Generic, Show)

data ConstraintSchema = ConstraintSchema {
     functions :: [FunctionSchema],
     predicates :: [PredicateSchema]
} deriving (Generic, Show)

data ObjectSchema = ObjectSchema {
     objType :: String,
     objName :: String
} deriving (Generic, Show)

data SubSchema = SubSchema {
           objects :: [ObjectSchema]
         , constraints  :: ConstraintSchema
         } deriving (Generic, Show)

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

toSchema ::
  ([ObjectSchema], [FunctionSchema], [PredicateSchema])
  -> SubStmt -> ([ObjectSchema], [FunctionSchema], [PredicateSchema])
toSchema acc@(objSchs, fnSchs, predSchs) subLine =
         case subLine of
         Decl t v -> 
              let res = ObjectSchema { objType = "", objName = "" }
              in (res : objSchs, fnSchs, predSchs)
         DeclList t vs -> error "TODO: export to JSON"

         Bind v (ApplyFunc f) -> 
              let res = FunctionSchema { varName = "", fname = "", fargNames = [] }
              in (objSchs, res : fnSchs, predSchs)
         Bind v (ApplyValCons f) -> 
              let res = FunctionSchema { varName = "", fname = "", fargNames = [] }
              in (objSchs, res : fnSchs, predSchs)

         ApplyP p -> 
              let res = PredicateSchema { pname = "", pargNames = [] }
              in (objSchs, fnSchs, res : predSchs)

         -- TODO: not sent to plugins
         Bind _ (VarE _) -> acc
         Bind _ (DeconstructorE _) -> acc
         EqualE _ _ -> acc
         EqualQ _ _ -> acc
         LabelDecl _ _ -> acc
         AutoLabel _ -> acc
         NoLabel _ -> acc

-- | Turn a Substance prog into the schema format defined above
subToSchema :: SubProg -> SubSchema
subToSchema prog = 
            let (objSchs, fnSchs, predSchs) = foldl toSchema ([], [], []) prog
            in SubSchema { 
                      objects = objSchs,
                      constraints = ConstraintSchema {
                                  functions = fnSchs,
                                  predicates = predSchs
                      }
            }

writeSubstanceToJSON :: FilePath -> SubOut -> IO ()
writeSubstanceToJSON file (SubOut subprog envs labels) = do
     let substanceSchema = subToSchema subprog
     let bytestr         = encode substanceSchema
     BL.writeFile file bytestr

--------------------------------------------------------

-- | Test writing a Substance program in JSON format
main :: IO ()
main = do
     let info = SubSchema { objects = [ ObjectSchema { objType = "Set", objName = "A" } ], 
                            constraints = ConstraintSchema
                                          { functions = [ FunctionSchema { varName = "C",
                                                      fname ="Union", 
                                                      fargNames =["A", "B"] } ],
                                            predicates = [ PredicateSchema { 
                                                       pname = "IsSubset", pargNames = ["C", "p"]}]
                                          }
                          }
     let res = encode info
     BL.writeFile "testSubstanceJSON.json" res
