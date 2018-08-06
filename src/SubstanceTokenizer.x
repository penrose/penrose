{
module SubstanceTokenizer where
}
%wrapper "basic"
$digit = 0-9
$alpha = [a-zA-Z]
tokens :-
  -- digits
  -- alphabetic characters
  [\ \t\f\v\r]+     {\s -> Space}
  [\n]+             {\s -> NewLine}
  "--".*            ;
  $digit+           { \s -> Int (read s) }
  [\+\-\*\/]        { \s -> Sym (head s) }
  \<->              { \s -> PredEq }
  \=                { \s -> ExprEq }
  \:=               { \s -> Bind }
  \(                { \s -> Lparen }
  \)                { \s -> Rparen }
  \,                { \s -> Comma }
  $alpha [$alpha $digit \_ \’]*   { \s -> Var s }
{
-- Each action has type :: String -> Token
-- The token type:
data Token =
  Bind |
  NewLine |
  PredEq |
  ExprEq |
  Comma |
  Let     |
  Lparen |
  Rparen |
  Space  |
  In      |
  Sym Char  |
  Var String  |
  Int Int
  deriving (Eq,Show)
main = do
  s <- getContents
  print (alexScanTokens s)
}
