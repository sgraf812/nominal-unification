{
module Lexer (lex) where

import Prelude hiding (lex)
import Token
}

%wrapper "basic"

$alpha = [a-zA-Z]
$digit = [0-9]

tokens :-
  $white+                         ;
  =>                              { \_ -> TokenEqGt }
  \.                              { \_ -> TokenEqGt }
  \(                              { \_ -> TokenLParen }
  \)                              { \_ -> TokenRParen }
  [A-Z]$digit*                    { \s -> TokenVar s }
  $alpha [$alpha $digit \_ \']*   { \s -> TokenAtom s }
  \~                              { \_ -> TokenTilde }

{
lex :: String -> [Token]
lex = alexScanTokens
}