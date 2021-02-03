{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS_HADDOCK prune #-}

module Penrose.Serializer where

import           Penrose.Env
import           Penrose.Substance
import           Penrose.GenOptProblem
import           Penrose.Optimizer

-- import           Penrose.Plugins
import           Penrose.Style
import           Penrose.SubstanceTokenizer
import           Penrose.Util

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char                  (toLower)
import           Data.Data
import qualified Data.Map.Strict            as M
import           Data.Proxy
import           Data.String
import           Data.Typeable
import           GHC.Generics
import qualified Numeric.LinearAlgebra      as L
import           System.Random              (StdGen)
import           Text.Megaparsec

--------------------------------------------------------------------------------
-- Packet serialization for server
data Packet a = Packet
  { packetType     :: String
  , packetSession  :: Maybe String
  , packetContents :: a
  }

$(deriveJSON
    defaultOptions
    {fieldLabelModifier = map toLower . drop 6, omitNothingFields = True}
    ''Packet)

data Request a = Request
  { requestSession :: Maybe String
  , requestCall    :: a
  }

$(deriveJSON
    defaultOptions
    {fieldLabelModifier = map toLower . drop 7, omitNothingFields = True}
    ''Request)

--------------------------------------------------------------------------------
instance ToJSONKey TypeVar

instance FromJSONKey TypeVar

instance ToJSONKey T

instance FromJSONKey T

instance ToJSONKey Var

instance FromJSONKey Var

--------------------------------------------------------------------------------
-- TODO: slowly move all the JSON decls other than the ones in Server to
--       this module
deriveJSON defaultOptions ''SourcePosition

deriveJSON defaultOptions ''Pos

deriveJSON defaultOptions ''SourcePos

deriveJSON defaultOptions ''TagExpr

deriveJSON defaultOptions ''FieldExpr

deriveJSON defaultOptions ''Translation

deriveJSON defaultOptions ''Expr

deriveJSON defaultOptions ''AnnoFloat

deriveJSON defaultOptions ''BinaryOp

deriveJSON defaultOptions ''UnaryOp

deriveJSON defaultOptions ''PropertyDecl

deriveJSON defaultOptions ''Path

deriveJSON defaultOptions ''Var

deriveJSON defaultOptions ''BindingForm

deriveJSON defaultOptions ''StyVar

deriveJSON defaultOptions ''VarEnv

deriveJSON defaultOptions ''TypeVar

deriveJSON defaultOptions ''T

deriveJSON defaultOptions ''TypeCtorApp

deriveJSON defaultOptions ''TypeConstructor

deriveJSON defaultOptions ''Arg

deriveJSON defaultOptions ''K

deriveJSON defaultOptions ''ValConstructor

deriveJSON defaultOptions ''Y

deriveJSON defaultOptions ''Type

deriveJSON defaultOptions ''Operator

deriveJSON defaultOptions ''PredicateEnv

deriveJSON defaultOptions ''StmtNotationRule

deriveJSON defaultOptions ''Predicate1

deriveJSON defaultOptions ''Prop

deriveJSON defaultOptions ''Predicate2

deriveJSON defaultOptions ''Penrose.SubstanceTokenizer.Token

-- TODO: de-lambdaize this to make it serializable
-- deriveJSON defaultOptions ''Policy
deriveJSON defaultOptions ''Params

deriveJSON defaultOptions ''Fn

deriveJSON defaultOptions ''StdGen

deriveJSON defaultOptions ''PolicyParams

deriveJSON defaultOptions ''OptType

deriveJSON defaultOptions ''OptStatus

deriveJSON defaultOptions ''BfgsParams

deriveJSON defaultOptions ''Penrose.GenOptProblem.State

--------------------------------------------------------------------------------
-- Interface
deriveJSON defaultOptions ''CompilerError

deriveJSON defaultOptions ''RuntimeError
--------------------------------------------------------------------------------
-- Plugins

-- Style
deriveJSON defaultOptions ''Header

deriveJSON defaultOptions ''Penrose.Style.Selector

deriveJSON defaultOptions ''DeclPattern

deriveJSON defaultOptions ''RelationPattern

deriveJSON defaultOptions ''Predicate

deriveJSON defaultOptions ''PredArg

deriveJSON defaultOptions ''StyT

deriveJSON defaultOptions ''STypeVar

deriveJSON defaultOptions ''STypeCtor

deriveJSON defaultOptions ''SArg

deriveJSON defaultOptions ''SelExpr

deriveJSON defaultOptions ''Stmt

deriveJSON defaultOptions ''StyType

deriveJSON defaultOptions ''SubOut

deriveJSON defaultOptions ''SubEnv

-- deriveJSON defaultOptions ''TypeConstructor

-- deriveJSON defaultOptions ''K

-- deriveJSON defaultOptions ''T

-- deriveJSON defaultOptions ''Y

-- deriveJSON defaultOptions ''Arg

-- deriveJSON defaultOptions ''TypeVar

deriveJSON defaultOptions ''SubExpr

deriveJSON defaultOptions ''SubPredicate

-- deriveJSON defaultOptions ''ValConstructor

-- deriveJSON defaultOptions ''Type

-- deriveJSON defaultOptions ''TypeCtorApp

deriveJSON defaultOptions ''Func

-- deriveJSON defaultOptions ''Operator

deriveJSON defaultOptions ''Deconstructor

-- deriveJSON defaultOptions ''PredicateEnv

deriveJSON defaultOptions ''SubPredArg

deriveJSON defaultOptions ''LabelOption

deriveJSON defaultOptions ''SubStmt

-- deriveJSON defaultOptions ''StmtNotationRule

-- deriveJSON defaultOptions ''Predicate1

-- deriveJSON defaultOptions ''Predicate2

-- deriveJSON defaultOptions ''Token

-- deriveJSON defaultOptions ''Prop
