{
module SubstanceTokenizer where
}
%wrapper "basic"
$digit = 0-9
$alpha = [a-zA-Z]
tokens :-
  -- digits
  -- alphabetic characters
  "--".*                            { \s -> Comment s }
  [\ \t\f\v\r]+                    {\s -> Space}
  [\n]+                             {\s -> NewLine}
  \<\-\>                              { \s -> PredEq }
  \=                                { \s -> ExprEq }
  \:=                               { \s -> Bind }
  \(                                { \s -> Lparen }
  \)                                { \s -> Rparen }
  \,                                { \s -> Comma }
  [\=\+\-\*\/\:\∈\←\→\↑\↓\↔\↕\↖\↗\↘\↙\↚\↛\↮\⟵\⟶\⟷\<\>\|\;\~\`\!\#\%\&\*\±\§\?\$] { \s -> Sym (head s) }
  $alpha [$alpha $digit \_ \’]*     { \s -> Var s }
  $alpha \`[$alpha $digit \_ \’]*   { \s -> Pattern s }

{
-- Each action has type :: String -> Token
-- The token type:
data Token =
  Bind              |
  NewLine           |
  PredEq            |
  ExprEq            |
  Comma             |
  Lparen            |
  Rparen            |
  Space             |
  Sym Char          |
  Var String        |
  Pattern String    |
  DSLLEntity String |
  Comment String
  deriving (Eq,Show)
main = do
  s <- getContents
  print (alexScanTokens s)
}
