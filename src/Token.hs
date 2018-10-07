module Token where

data Token
  = TokenVar String
  | TokenAtom String
  | TokenEqGt
  | TokenLParen
  | TokenRParen
  | TokenTilde
  deriving (Eq, Show)
