{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Penrose.Pretty where

import           Penrose.Env
import           Penrose.Substance
import           Text.PrettyPrint.HughesPJClass hiding (braces, colon, comma,
                                                 parens)

prettySubstance :: SubProg -> Doc
prettySubstance = vcat . map pPrint

instance Pretty SubStmt where
  pPrint (Decl t v) = pPrint t <+> pPrint v

instance Pretty T where
  pPrint (TTypeVar t) = text (typeVarName t)

instance Pretty Var where
  pPrint (VarConst v) = text v
--   | Bind Var
--          Expr
--   | EqualE Expr
--            Expr
--   | EqualQ Predicate
--            Predicate
--   | ApplyP Predicate
--   | LabelDecl Var
--               String
--   | AutoLabel LabelOption
--   | NoLabel [Var]
