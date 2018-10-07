{
module Parser (parse) where

import           Bound
import           Data.Void
import           Numeric.Natural
import           Syntax
import           Token

}

%name parse
%tokentype { Token }
%error { parseError }

%token
  atom            { TokenAtom $$ }
  var             { TokenVar $$ }
  "=>"            { TokenEqGt }
  '('             { TokenLParen }
  ')'             { TokenRParen }
  '~'             { TokenTilde }

%right "abs"
%right atom var '('

%%

Equation :: { Equation }
  : Expr '~' Expr { Eq $1 $3 }

Expr :: { Expr }
  : NoApp { $1 }
  | Expr NoApp { App $1 $2 }

NoApp :: { Expr }
  : '(' Expr ')' { $2 }
  | atom { Atom (A $1) }
  | var { Var (V $1) }
  | atom "=>" Expr %prec "abs" { Abs (A $1) $3 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}