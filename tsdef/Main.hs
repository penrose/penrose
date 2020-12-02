{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Data.Aeson.TH
import           Data.Aeson.TypeScript.TH
import           Data.Map.Strict
import           Data.Proxy
import           Data.String.Interpolate.IsString
import           Penrose.Env
import           Penrose.GenOptProblem
import           Penrose.Shapes                   hiding (Shape)
import           Penrose.Style
import           Penrose.Substance
import           Penrose.SubstanceTokenizer       (Token)
import           Penrose.Transforms
import           Text.Megaparsec                  (Pos, SourcePos)

data Shape =
  Shape
    { shapeType  :: String
    , properties :: Map PropID (Value Double)
    }

$(deriveTypeScript defaultOptions ''Shape)

$(deriveTypeScript defaultOptions ''ArgVal)

$(deriveTypeScript defaultOptions ''Translation)

$(deriveTypeScript defaultOptions ''FieldExpr)

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

$(deriveTypeScript defaultOptions ''BfgsParams)

$(deriveTypeScript defaultOptions ''Params)

$(deriveTypeScript defaultOptions ''OptStatus)

-- All the varenv types
$(deriveTypeScript defaultOptions ''Header)

$(deriveTypeScript defaultOptions ''Selector)

$(deriveTypeScript defaultOptions ''DeclPattern)

$(deriveTypeScript defaultOptions ''RelationPattern)

$(deriveTypeScript defaultOptions ''Predicate)

$(deriveTypeScript defaultOptions ''PredArg)

$(deriveTypeScript defaultOptions ''StyT)

$(deriveTypeScript defaultOptions ''STypeVar)

$(deriveTypeScript defaultOptions ''STypeCtor)

$(deriveTypeScript defaultOptions ''SArg)

$(deriveTypeScript defaultOptions ''SelExpr)

$(deriveTypeScript defaultOptions ''SourcePos)

$(deriveTypeScript defaultOptions ''Pos)

$(deriveTypeScript defaultOptions ''Stmt)

$(deriveTypeScript defaultOptions ''StyType)

$(deriveTypeScript defaultOptions ''SubOut)

$(deriveTypeScript defaultOptions ''SubEnv)

$(deriveTypeScript defaultOptions ''VarEnv)

$(deriveTypeScript defaultOptions ''TypeConstructor)

$(deriveTypeScript defaultOptions ''K)

$(deriveTypeScript defaultOptions ''T)

$(deriveTypeScript defaultOptions ''Y)

$(deriveTypeScript defaultOptions ''Arg)

$(deriveTypeScript defaultOptions ''TypeVar)

$(deriveTypeScript defaultOptions ''SubExpr)

$(deriveTypeScript defaultOptions ''SubPredicate)

$(deriveTypeScript defaultOptions ''ValConstructor)

$(deriveTypeScript defaultOptions ''Type)

$(deriveTypeScript defaultOptions ''TypeCtorApp)

$(deriveTypeScript defaultOptions ''Func)

$(deriveTypeScript defaultOptions ''Operator)

$(deriveTypeScript defaultOptions ''Deconstructor)

$(deriveTypeScript defaultOptions ''PredicateEnv)

$(deriveTypeScript defaultOptions ''SubPredArg)

$(deriveTypeScript defaultOptions ''LabelOption)

$(deriveTypeScript defaultOptions ''SubStmt)

$(deriveTypeScript defaultOptions ''StmtNotationRule)

$(deriveTypeScript defaultOptions ''Predicate1)

$(deriveTypeScript defaultOptions ''Predicate2)

$(deriveTypeScript defaultOptions ''Token)

$(deriveTypeScript defaultOptions ''Prop)

-- NOTE: omitting all the varenv related types because the evaluator doesn't need them
instance (TypeScript a, TypeScript b) => TypeScript (Map a b) where
  getTypeScriptType _ =
    [i|{[k: #{getTypeScriptType (Proxy :: Proxy a)}]: #{getTypeScriptType (Proxy :: Proxy b)}}|]

--   getTypeScriptType _ =
--     "Map<" <> getTypeScriptType (Proxy :: Proxy a) <> ", " <>
--     getTypeScriptType (Proxy :: Proxy b) <>
--     ">"
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
     (getTypeScriptDeclarations (Proxy :: Proxy Elem)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy OptStatus)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy BfgsParams)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Params)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Header)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Selector)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy DeclPattern)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy RelationPattern)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Predicate)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy PredArg)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy StyT)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy STypeVar)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy STypeCtor)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy SArg)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy SelExpr)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy SourcePos)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Pos)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Stmt)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy StyType)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy SubOut)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy SubEnv)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy VarEnv)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy TypeConstructor)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy K)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy T)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Y)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Arg)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy TypeVar)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy SubExpr)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy SubPredicate)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy ValConstructor)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Type)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy TypeCtorApp)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Func)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Operator)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Deconstructor)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy PredicateEnv)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy SubPredArg)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy LabelOption)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy SubStmt)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy StmtNotationRule)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Predicate1)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Predicate2)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Token)) <>
     (getTypeScriptDeclarations (Proxy :: Proxy Prop)))
