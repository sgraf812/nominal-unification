{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Machines.RhoMachine where

import           Syntax
import           Constraints
import           Control.Monad                  ( foldM )
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Except
import           Data.List                      ( foldl' )

newtype RhoMachine a
  = RhoMachine (StateT Int (Except UnificationError) a)
  deriving (Functor, Applicative, Monad)

runRhoMachine :: RhoMachine a -> Except UnificationError a
runRhoMachine (RhoMachine m) = evalStateT m 0

throw :: UnificationError -> RhoMachine a
throw = RhoMachine . lift . throwE

freshVar :: RhoMachine Var
freshVar = RhoMachine $ state $ \n -> (V ("$X" ++ show n), n + 1)

freshAtom :: RhoMachine Atom
freshAtom = RhoMachine $ state $ \n -> (A ("$a" ++ show n), n + 1)

eval
  :: NuProblem
  -> DeltaProblem
  -> Substitution
  -> RhoProblem
  -> RhoMachine (NuProblem, DeltaProblem, Substitution)
eval np dp s = foldM step (np, dp, s)
 where
  step (np, dp, s) (EE cl@(Clo el bml) cr@(Clo er bmr)) = case (el, er) of
    (Atom al  , Atom ar  ) -> pure (AA (Clo al bml) (Clo ar bmr) : np, dp, s)
    (Atom al  , Var vr   ) -> pure (AV (Clo al bml) (Clo vr bmr) : np, dp, s)
    (Var  _   , Atom _   ) -> step (np, dp, s) (EE cr cl)
    (Var  vl  , Var vr   ) -> pure (np, VV (Clo vl bml) (Clo vr bmr) : dp, s)
    (App l1 l2, App r1 r2) -> do
      (np', dp', s') <- step (np, dp, s) (EE (Clo l1 bml) (Clo r1 bmr))
      step (np', dp', s') (EE (Clo l2 bml) (Clo r2 bmr))
    (Abs al bl, Abs ar br) -> do
      let bml' = extend al bml
      let bmr' = extend ar bmr
      step (np, dp, s) (EE (Clo bl bml') (Clo br bmr'))
    (Var vl, App r1 r2) -> do
      v1 <- freshVar
      v2 <- freshVar
      let s' = extendSubst vl (App (Var v1) (Var v2)) s
      (np', dp', s'') <- step (np, dp, s') (EE (Clo (Var v1) bml) (Clo r1 bmr))
      step (np', dp', s'') (EE (Clo (Var v2) bml) (Clo r2 bmr))
    (App _ _, Var _    ) -> step (np, dp, s) (EE cr cl)
    (Var vl , Abs ar br) -> do
      al <- freshAtom
      vb <- freshVar
      let s'   = extendSubst vl (Abs al (Var vb)) s
      let bml' = extend al bml
      let bmr' = extend ar bmr
      step (np, dp, s') (EE (Clo (Var vb) bml') (Clo br bmr'))
    (Abs _ _, Var _) -> step (np, dp, s) (EE cr cl)
    _                -> throw (EEMismatch cl cr)
