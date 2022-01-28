module Repl where

import System.Console.Haskeline
import System.Environment
import System.Exit
import Control.Applicative
import Control.Monad.State
import Data.Set as S
import Data.Map as M
import Data.List as L
import Data.Text as T

import Utils
import TypeInfer
import Parse (parseExpr)

-------------------------------------------------------------------------------
-- Completion
-------------------------------------------------------------------------------

cmdPrefix :: [String]
cmdPrefix = [":define",":load",":search",":quit"]

searchFunc :: String -> [Completion]
searchFunc str = L.map simpleCompletion $
                        L.filter (L.isPrefixOf str) cmdPrefix

mySettings :: Monad m => Settings m
mySettings = Settings {
      historyFile = Just "history.his"
    , complete = completeWord Nothing " \t" (return . searchFunc)
    , autoAddHistory = True
}

-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------
type IState = M.Map String Expr
type Repl a = InputT (StateT IState IO) a

prompt :: String
prompt = "> "

repl :: Repl ()
repl = do
    input <- handleInterrupt
        (outputStrLn "Interupted!" >> liftIO exitSuccess)
        (getInputLine prompt)
    case input of
        Nothing ->  do
            outputStrLn "unkown command!"
        Just r -> do
            case parseExpr r of
                Right t -> do
                    lift (modify $ M.insert r t)
                    typeCheck t
                Left err -> liftIO $ print err
            s <- lift get 
            liftIO $ print s
            outputStrLn r
    repl

shell :: Show a => Repl a -> IO ()
shell r = do
    result <- evalStateT (runInputT mySettings $ withInterrupt r) M.empty 
    print result


-------------------------------------------------------------------------------
-- Other
-------------------------------------------------------------------------------
typeCheck :: Expr -> Repl ()
typeCheck expr =
    let ty = typeInferTop expr
    in do
        outputStrLn "typecheck:"
        outputStr $ T.unpack $ showResult ty
