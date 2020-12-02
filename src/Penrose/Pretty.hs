{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Penrose.Pretty where

import           Penrose.Element                (parseElement)
import           Penrose.Env                    hiding (comma, dollar, dollars,
                                                 parens)
import           Penrose.Substance
import           Prelude                        hiding ((<>))
import           Text.PrettyPrint.HughesPJClass

prettySubstance :: SubProg -> Doc
prettySubstance = vcat . map pPrint

instance Pretty SubStmt where
  pPrint (Decl t v) = pPrint t <+> pPrint v
  pPrint (Bind v e) = pPrint v <+> text ":=" <+> pPrint e
  pPrint (ApplyP p) = pPrint p
  pPrint (AutoLabel Default) = text "AutoLabel" <+> text "All"
  pPrint (AutoLabel (IDs vs)) = text "AutoLabel" <+> map pPrint vs `sepBy` comma
  pPrint (NoLabel vs) = text "NoLabel" <+> map pPrint vs `sepBy` comma
  pPrint (LabelDecl v s) = text "Label" <+> pPrint v <+> dollars (text s)

instance Pretty SubExpr where
  pPrint (VarE v)         = pPrint v
  pPrint (ApplyFunc f)    = pPrint f
  pPrint (ApplyValCons f) = pPrint f
  -- pPrint (DeconstructorE d) = pPrint f

instance Pretty SubPredicate where
  pPrint SubPredicate {predicateName = n, predicateArgs = args} =
    pPrint n <> parens (map pPrint args `sepBy` (comma <> space))

instance Pretty SubPredArg where
  pPrint (PE expr) = pPrint expr
  pPrint (PP p)    = pPrint p

instance Pretty Func where
  pPrint Func {nameFunc = name, argFunc = args} =
    text name <> parens (map pPrint args `sepBy` comma)

instance Pretty T where
  pPrint (TTypeVar t) = text (typeVarName t)
  pPrint (TConstr TypeCtorApp {nameCons = n, argCons = []}) = text n
  pPrint (TConstr TypeCtorApp {nameCons = n, argCons = args}) =
    text n <> parens (map pPrint args `sepBy` comma <> space)

instance Pretty Arg where
  pPrint (AVar v) = pPrint v
  pPrint (AT t)   = pPrint t

instance Pretty Var where
  pPrint (VarConst v) = text v

sepBy :: [Doc] -> Doc -> Doc
sepBy ds punc = hcat $ punctuate punc ds

dollar :: Doc
dollar = text "$"

dollars :: Doc -> Doc
dollars d = dollar <> d <> dollar

--   | EqualE Expr
--            Expr
--   | EqualQ SubPredicate
--            SubPredicate
--   | LabelDecl Var
--               String
--   | AutoLabel LabelOption
--   | NoLabel [Var]
-- Penrose.Pretty.test "examples/set-theory-domain/tree.sub" "examples/set-theory-domain/setTheory.dsl"
--
test :: String -> String -> IO ()
test subFile elmFile = do
  subIn <- readFile subFile
  elmIn <- readFile elmFile
  case parseElement "" elmIn of
    Left e -> error $ show e
    Right env ->
      case parseSubstance "" subIn env of
        Left e -> error $ show e
        Right (SubOut prog _ _) -> do
          print prog
          print $ prettySubstance prog
  return ()
