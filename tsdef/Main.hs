{-# LANGUAGE TemplateHaskell, ScopedTypeVariables           #-}
{-# LANGUAGE QuasiQuotes               #-}
module Main where

import Data.Proxy
import Data.Monoid
import Data.Aeson.TH
import Data.Aeson.TypeScript.TH
import Data.Map.Strict
import           Penrose.Style
import           Penrose.Env
import           Penrose.SubstanceTokenizer
import           Penrose.Util

$(deriveTypeScript defaultOptions ''Expr)

$(deriveTypeScript defaultOptions ''AnnoFloat)

$(deriveTypeScript defaultOptions ''BinaryOp)

$(deriveTypeScript defaultOptions ''UnaryOp)

$(deriveTypeScript defaultOptions ''PropertyDecl)

$(deriveTypeScript defaultOptions ''Path)

$(deriveTypeScript defaultOptions ''Var)

$(deriveTypeScript defaultOptions ''BindingForm)

$(deriveTypeScript defaultOptions ''StyVar)

-- instance (TypeScript a, TypeScript b) => TypeScript (Map a b) where
--   getTypeScriptType _ = [i|{[k: #{getTypeScriptType (Proxy :: Proxy a)}]: #{getTypeScriptType (Proxy :: Proxy b)}}|]

main = putStrLn $ formatTSDeclarations (
  (getTypeScriptDeclarations (Proxy :: Proxy Expr)) <>
  (getTypeScriptDeclarations (Proxy :: Proxy AnnoFloat)) <>
  (getTypeScriptDeclarations (Proxy :: Proxy BinaryOp)) <>
  (getTypeScriptDeclarations (Proxy :: Proxy UnaryOp)) <>
  (getTypeScriptDeclarations (Proxy :: Proxy PropertyDecl)) <>
  (getTypeScriptDeclarations (Proxy :: Proxy Path)) <>
  (getTypeScriptDeclarations (Proxy :: Proxy Var)) <>
  (getTypeScriptDeclarations (Proxy :: Proxy BindingForm)) <>
  (getTypeScriptDeclarations (Proxy :: Proxy StyVar)) 
  )