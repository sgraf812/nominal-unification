{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Machines.NuMachine where

import           Syntax
import           Constraints
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Except

newtype NuMachine a
  = NuMachine (StateT Substitution (Except UnificationError) a)
  deriving (Functor, Applicative, Monad)

runNuMachine
  :: Substitution -> NuMachine a -> Except UnificationError Substitution
runNuMachine s (NuMachine m) = execStateT m s

throw :: UnificationError -> NuMachine a
throw = NuMachine . lift . throwE

bind :: Var -> Expr -> NuMachine ()
bind v = NuMachine . modify . extendSubst v

eval :: NuProblem -> NuMachine ()
eval = mapM_ step
 where
  step (AA clo1 clo2) | clo1 ~~ clo2 = pure ()
                      | otherwise    = throw (AAMismatch clo1 clo2)
  step (AV (Clo a bm1) (Clo v bm2)) = case lookupAtom a bm1 of
    Free _ -> case lookupAtom a bm2 of
      Free _ -> bind v (Atom a)
      _      -> throw (NameCapture a bm1)
    Bound _ i -> case lookupIdx i bm2 of
      Just a2 -> bind v (Atom a2)
      _       -> throw (NoMatchingBinder i bm2)
