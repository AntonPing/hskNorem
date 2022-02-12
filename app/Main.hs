module Main where
import Parse
import Super
import TypeInfer (typeInferTop, inferTest)
import Compile (codegenTop)
import Repl
import System.Environment
import Control.Monad

import Data.Map as M

sdsd

main :: IO ()
main = do
    args <- getArgs
    void repl
    {-
    case args of
        [] -> 
        [fname] -> shell (load [fname])
        ["test", fname] -> shell (load [fname] >> browse [] >> quit ())
        _ -> putStrLn "invalid arguments"
    -}    