{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Data.Aeson.TH
import           Data.Aeson.TypeScript.TH
import           Data.Map.Strict
import           Data.Monoid
import           Data.Proxy
import           Penrose.Env
import           Penrose.Shapes                   hiding (Shape)
import           Penrose.Style
import           Penrose.SubstanceTokenizer
import           Penrose.Transforms
import           Penrose.Util

-- import Data.Aeson.TypeScript.Types
import           Data.Data
import qualified Data.List                        as L
import           Data.Set
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as T
import qualified Data.Text.Lazy                   as TL

data Shape = Shape
  { shapeName  :: String
  , properties :: Map PropID (Value Double)
  }

$(deriveTypeScript defaultOptions ''Shape)

$(deriveTypeScript defaultOptions ''ArgVal)

$(deriveTypeScript defaultOptions ''Translation)

$(deriveTypeScript defaultOptions ''FieldExpr)

-- $(deriveTypeScript defaultOptions ''Name)
$(deriveTypeScript defaultOptions ''TagExpr)

$(deriveTypeScript defaultOptions ''Expr)

$(deriveTypeScript defaultOptions ''AnnoFloat)

$(deriveTypeScript defaultOptions ''BinaryOp)

$(deriveTypeScript defaultOptions ''UnaryOp)

$(deriveTypeScript defaultOptions ''PropertyDecl)

$(deriveTypeScript defaultOptions ''Path)

$(deriveTypeScript defaultOptions ''Var)

$(deriveTypeScript defaultOptions ''BindingForm)

$(deriveTypeScript defaultOptions ''StyVar)

$(deriveTypeScript defaultOptions ''Value)

$(deriveTypeScript defaultOptions ''SubPath)

$(deriveTypeScript defaultOptions ''HMatrix)

$(deriveTypeScript defaultOptions ''Color)

$(deriveTypeScript defaultOptions ''Elem)

-- NOTE: omitting all the varenv related types because the evaluator doesn't need them
instance (TypeScript a, TypeScript b) => TypeScript (Map a b) where
  getTypeScriptType _ =
    "Map<" <> getTypeScriptType (Proxy :: Proxy a) <> ", " <>
    getTypeScriptType (Proxy :: Proxy b) <>
    ">"

--   getTypeScriptType _ =
--     [i|{[k: #{getTypeScriptType (Proxy :: Proxy a)}]: #{getTypeScriptType (Proxy :: Proxy b)}}|]
main =
  putStrLn $
  formatTSDeclarations
    ((getTypeScriptDeclarations (Proxy :: Proxy Shape)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy ArgVal)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Translation)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy FieldExpr)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy TagExpr)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Name)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Expr)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy AnnoFloat)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy BinaryOp)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy UnaryOp)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy PropertyDecl)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Path)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Var)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy BindingForm)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy StyVar)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Value)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy SubPath)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy HMatrix)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Color)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Elem)))
