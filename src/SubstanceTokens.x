{
module SubstanceTokenizer where
}
%wrapper "basic"
$digit = 0-9
$alpha = [a-zA-Z]
$symbol = [\=\+\-\*\/\:\∈\←\→\↑\↓\↔\↕\↖\↗\↘\↙\↚\↛\↮\⟵\⟶\⟷\<\>\|\;\~\`\!\#\%\&\*\±\§\?\$\<\>\⊆\∪\∩\`\∫\[\]\^]
tokens :-
  -- digits
  -- alphabetic characters
  "Label".*                         { \s -> Label s }
  "AutoLabel".*                     { \s -> AutoLabel s }
  "--".*                            { \s -> Comment s }
  "/*".*                            { \s -> StartMultiComment s}
  "*/".*                            { \s -> EndMultiComment s}
  [\ \t\f\v\r]+                     {\s -> Space}
  [\n]+                             {\s -> NewLine}
  \;                                {\s -> NewLine}
  [\.]{2}                           {\s -> RecursivePatternElement []}
  [\.]{3}                           {\s -> RecursivePattern []}
  \<\-\>                            { \s -> PredEq }
  \=                                { \s -> ExprEq }
  \:=                               { \s -> Bind }
  \(                                { \s -> Lparen }
  \)                                { \s -> Rparen }
  \,                                { \s -> Comma }
  \[\.\.\.\]                        { \s -> Iterator }
  \[\.\]                            { \s -> Iter }
  $symbol                           { \s -> Sym (head s) }
  $alpha [$alpha $digit \_ \’ \.]*     { \s -> Var s }
  $alpha \`[$alpha $digit \_ \’]*   { \s -> Pattern s False }

{
-- Each action has type :: String -> Token
-- The token type:
data Token =
  Bind                            |
  Iterator                        |
  Iter                            |
  NewLine                         |
  PredEq                          |
  ExprEq                          |
  Comma                           |
  Lparen                          |
  Rparen                          |
  Space                           |
  Sym Char                        |
  Var String                      |
  RecursivePattern [Token]        |
  RecursivePatternElement [Token] |
  SinglePatternElement [Token]    |
  Pattern String Bool             |
  Entitiy String                  |
  DSLLEntity String               |
  Label String                    |
  AutoLabel String                |
  Comment String                  |
  StartMultiComment String        |
  EndMultiComment String
  deriving (Show)

instance Eq Token where
  Bind == Bind = True
  NewLine == NewLine = True
  PredEq == PredEq = True
  ExprEq == ExprEq = True
  Comma == Comma = True
  Lparen == Lparen = True
  Rparen == Rparen = True
  Space == Space = True
  Sym c1 == Sym c2 = c1 == c2
  Var c1 == Var c2 = c1 == c2
  Pattern c1 b1 == Pattern c2 b2 = True
  Entitiy c1 == Entitiy c2 = c1 == c2
  DSLLEntity c1 == DSLLEntity c2 = c1 == c2
  Comment c1 == Comment c2 = c1 == c2
  StartMultiComment c1 == StartMultiComment c2 = c1 == c2
  EndMultiComment c1 == EndMultiComment c2 = c1 == c2
  RecursivePatternElement lst1 == RecursivePatternElement lst2 = True
  RecursivePattern lst1 == RecursivePattern lst2 = lst1 == lst2
  a == b = False


main = do
  s <- getContents
  print (alexScanTokens s)
}
