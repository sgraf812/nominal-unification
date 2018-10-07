{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Syntax where

import           Control.Monad.Trans.State.Strict
import           Control.Monad                  ( guard )
import           Data.Functor                   ( ($>) )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IntMap

newtype Atom = A String
  deriving (Eq, Ord, Show)

newtype Var = V String
  deriving (Eq, Ord, Show)

data Expr
  = Atom Atom
  | Var Var
  | App Expr Expr
  | Abs Atom Expr
  deriving Show

data Equation
  = Eq Expr Expr
  deriving Show

data Boundness
  = Bound Atom Int
  | Free Atom

extend :: Atom -> BinderMap -> BinderMap
extend a bm = bm { a2i = Map.insert a (Map.size (a2i bm)) (a2i bm)
                 , i2a = IntMap.insert (Map.size (a2i bm)) a (i2a bm)
                 }

lookupAtom :: Atom -> BinderMap -> Boundness
lookupAtom a = maybe (Free a) (Bound a) . Map.lookup a . a2i

lookupIdx :: Int -> BinderMap -> Maybe Atom
lookupIdx i bm = do
  let j = Map.size (a2i bm) - i
  a  <- IntMap.lookup i (i2a bm)
  i' <- Map.lookup a (a2i bm)
  guard (i == i') $> a

emptyBinderMap :: BinderMap
emptyBinderMap = BM Map.empty IntMap.empty

data BinderMap
  = BM
  { a2i :: Map Atom Int
  , i2a :: IntMap Atom
  } deriving Show
