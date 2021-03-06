module Repl where

import qualified System.Console.Haskeline as HL
import System.Console.Repline

import System.Environment
import System.Exit
import Control.Applicative
import Control.Monad.State
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T

import Utils
import TypeInfer
import Parse (parseExpr)

type IState = M.Map String Expr
type Repl = HaskelineT (StateT IState IO)

-- Evaluation : handle each line user inputs

-- Options --
help :: Cmd Repl
help arg = liftIO $ print $ "Help: " ++ show arg

say :: Cmd Repl
say arg = do
    liftIO $ putStrLn $ "cowsay" ++ " " ++ arg
    return ()

quit :: Cmd Repl
quit arg = abort

eval :: Cmd Repl
eval arg = liftIO $ do 
    case parseExpr arg of
        Left err -> do
            putStrLn "parser failed."
            print err
        Right expr -> do
            print expr
            typeInferIO expr


-- Settings --

replBan :: MultiLine -> Repl String
replBan SingleLine = return "> "
replBan MultiLine = return "  "

replCmd :: Cmd Repl
replCmd = eval

replOpts :: Options Repl
replOpts =
    [ ("help", help)
    , ("say", say)
    , ("eval", eval)
    , ("quit", quit)
    ]

completer :: Monad m => WordCompleter m
completer n = do
  let names = fmap ((':' :) . fst) replOpts
  return $ L.filter (L.isPrefixOf n) names

replInit :: Repl ()
replInit = liftIO $ putStrLn "Welcome!"

replFinal :: Repl ExitDecision
replFinal = liftIO $ do
    putStrLn "Bye!"
    return Exit

setting :: ReplOpts (StateT IState IO)
setting = ReplOpts
  { banner           = replBan
  , command          = replCmd
  , options          = replOpts
  , prefix           = Just ':'
  , multilineCommand = Just "multi"
  , tabComplete      = Word0 completer
  , initialiser      = replInit
  , finaliser        = replFinal
  }

repl :: IO ((), IState)
repl = runStateT (evalReplOpts setting) M.empty