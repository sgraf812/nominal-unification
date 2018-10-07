module Unification where

import Constraints
import Syntax
import qualified Machines.NuMachine as Nu
import qualified Machines.DeltaMachine as Delta
import qualified Machines.RhoMachine as Rho
import           Control.Monad.Trans.Except

unify :: Expr -> Expr -> Either UnificationError (Substitution, DeltaProblem)
unify l r = runExcept $ do
  (np, dp, s1) <- Rho.runRhoMachine (Rho.eval [] [] idSubst [EE (Clo l emptyBinderMap) (Clo r emptyBinderMap)])
  s2 <- Nu.runNuMachine s1 (Nu.eval np)
  Delta.runDeltaMachine (Delta.eval s2 dp (dom s2))