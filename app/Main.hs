module Main where
import Parse
import Super
import TypeInfer (typeInferTop, inferTest)
import Compile (codegenTop)
import Repl
import System.Environment

import Data.Map as M

main :: IO ()
main = do
    putStrLn inferTest
    args <- getArgs
    case args of
        [] -> shell repl
        {-
        [fname] -> shell (load [fname])
        ["test", fname] -> shell (load [fname] >> browse [] >> quit ())
        -}
        _ -> putStrLn "invalid arguments"
    c <- getLine 
    case parseExpr c of
        Left e -> do 
            putStrLn "Parsing Error:"
            print e
        Right r -> do 
            putStrLn "Parsing Success:"
            print r
            putStrLn "Type Checking:"
            print $ typeInferTop r
            --let list = M.elems $ snd $ superTop r
            --print list
            --print (fmap codegenTop list)

    