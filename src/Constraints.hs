module Constraints where

import           Syntax
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

data Clo a
  = Clo a BinderMap
  deriving Show

sameClo :: Clo Atom -> Clo Atom -> Bool
sameClo (Clo a1 bm1) (Clo a2 bm2) =
  case (lookupAtom a1 bm1, lookupAtom a2 bm2) of
    (Free _    , Free _    ) -> a1 == a2
    (Bound _ i1, Bound _ i2) -> i1 == i2

(~~) :: Clo Atom -> Clo Atom -> Bool
(~~) = sameClo

data NuEquation
  = AA (Clo Atom) (Clo Atom)
  | AV (Clo Atom) (Clo Var)
  deriving Show

type NuProblem = [NuEquation]

data DeltaEquation
  = VV (Clo Var) (Clo Var)
  deriving Show

type DeltaProblem = [DeltaEquation]

data MultiEquation
  = EE (Clo Expr) (Clo Expr)
  deriving Show

type RhoProblem = [MultiEquation]

newtype Substitution = Subst (Map Var Expr)
  deriving (Show)

idSubst :: Substitution
idSubst = Subst Map.empty

dom :: Substitution -> [Var]
dom (Subst m) = Map.keys m

subst :: Expr -> Substitution -> Expr
subst (Var v) s@(Subst m) | Just e <- Map.lookup v m = subst e s
subst (Abs a e) s = Abs a (subst e s)
subst e         _ = e

extendSubst :: Var -> Expr -> Substitution -> Substitution
extendSubst v e (Subst m) = Subst (Map.insert v e m)

data UnificationError
  = AAMismatch (Clo Atom) (Clo Atom)
  | NameCapture Atom BinderMap
  | NoMatchingBinder Int BinderMap
  | EEMismatch (Clo Expr) (Clo Expr)
  deriving Show
