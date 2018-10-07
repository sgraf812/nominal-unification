module Main where

import Prelude hiding (lex)
import           Syntax
import           System.Console.Repline
import Control.Monad.IO.Class
import Unification (unify)
import Data.List (isPrefixOf)
import Lexer (lex)
import Parser (parse)

type Repl a = HaskelineT IO a

cmd :: String -> Repl ()
cmd input = liftIO $ print eq >> print res
  where
    eq@(Eq l r) = parse . lex $ input
    res = unify l r


completer :: Monad m => WordCompleter m
completer n = do
  let names = ["kirk", "spock", "mccoy"]
  return $ filter (isPrefixOf n) names

help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

options :: [(String, [String] -> Repl ())]
options =
  [ ("help", help)  -- :help
  , ("type", help)  -- :type
  ]

ini :: Repl ()
ini = return ()

main :: IO ()
main = evalRepl "λ> " cmd options (Word completer) ini
