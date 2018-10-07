{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Machines.DeltaMachine where

import           Syntax
import           Constraints
import           Control.Monad                  ( unless )
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Except
import           Data.List                      ( partition )

newtype DeltaMachine a
  = DeltaMachine (Except UnificationError a)
  deriving (Functor, Applicative, Monad)

runDeltaMachine :: DeltaMachine a -> Except UnificationError a
runDeltaMachine (DeltaMachine m) = m

throw :: UnificationError -> DeltaMachine a
throw = DeltaMachine . throwE

occursInEq :: Var -> DeltaEquation -> Bool
occursInEq x (VV (Clo a _) (Clo b _)) = x == a || x == b

eval
  :: Substitution
  -> DeltaProblem
  -> [Var]
  -> DeltaMachine (Substitution, DeltaProblem)
eval s p xs = case (xs, p) of
  ([]    , _ ) -> pure (s, p)
  (_     , []) -> pure (s, p)
  (x : xs, _ ) -> do
    let (w, wo) = partition (occursInEq x) p
    (s', xs') <- pull s xs w
    eval s' wo xs'

pull
  :: Substitution -> [Var] -> DeltaProblem -> DeltaMachine (Substitution, [Var])
pull s xs p = case p of
  [] -> pure (s, xs)
  VV (Clo x1 bm1) (Clo x2 bm2) : p' ->
    case (subst (Var x1) s, subst (Var x2) s) of
      (Atom a1, Atom a2) -> do -- NN

        let clo1 = Clo a1 bm1
        let clo2 = Clo a2 bm2
        unless (clo1 ~~ clo2) (throw (AAMismatch clo1 clo2))
        pull s xs p'
      (Var x1', Atom a2) -> do -- VN

        s' <- findSubstClo s x1' bm1 (Clo a2 bm2)
        pull s' (x1' : xs) p'
      (Atom a1, Var x2') -> do -- NV

        s' <- findSubstClo s x2' bm2 (Clo a1 bm1)
        pull s' (x2' : xs) p'

findSubstClo
  :: Substitution -> Var -> BinderMap -> Clo Atom -> DeltaMachine Substitution
findSubstClo s x bmx (Clo a bma) = case lookupAtom a bma of
  Free _ -> case lookupAtom a bmx of
    Free _ -> pure (extendSubst x (Atom a) s)
    _      -> throw (NameCapture a bmx)
  Bound _ i -> case lookupIdx i bmx of
    Just a2 -> pure (extendSubst x (Atom a2) s)
    _       -> throw (NoMatchingBinder i bmx)


