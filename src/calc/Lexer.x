{
module Lexer (main) where
}

%wrapper "basic"

words :-

$white+ ;
[A-Za-z0-9\'\-]+ { \s -> () }

{
main = do
 s <- getContents
 print (length (alexScanTokens s))
}
