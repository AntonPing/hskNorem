module Compile where
{-
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Text as T

import Utils


data ByteCode =
      PushFn Name
    | PushInt Int
    | PushReal Double
    | PushBool Bool
    | PushVar Name
    | CallFn Name
    | PopTo Name
    deriving (Ord,Eq,Show)

type Blocks = M.Map Name [ByteCode]

data GenState = GenState {
    block :: [ByteCode]
  , vars :: [Name]
}

type Codegen a = State GenState a

runCG :: Codegen a -> (a, GenState)
runCG m = runState m stateInit
  where
    stateInit = GenState {
        block = []
      , vars = []
    }

newVar :: [Name] -> Name -> Name
newVar xs x
    | x `elem` xs =
        let ys = fmap (\i ->  x <> T.pack (show i)) [0..]
        in  head $ filter (`notElem` xs) ys
    | otherwise = x

isBounded :: Name -> Codegen Bool
isBounded x = do
    s <- get
    return $ x `notElem` vars s

genNewVar :: Name -> Codegen Name
genNewVar x = do
    s <- get
    let new = newVar (vars s) x
    put s{ vars = new : vars s }
    return new

writeCode :: ByteCode -> Codegen ()
writeCode bc = do
    s <- get
    put s{ block = bc : block s}
    return ()

codegen :: Expr -> Codegen ()
codegen (EVar x) = writeCode $ PushVar x
codegen (ELam x t) = 
    error "No lambda should appear inside a toplevel supercombinator!"
codegen t0@(EApp _ _) =
    let (x:xs) = appUnfold t0
    in do
        mapM_ codegen (reverse xs)
        case x of
            EVar f -> writeCode $ CallFn f
            ELit (LFun f _) -> writeCode $ CallFn f
            _ -> error "Application should begin with a funciton!"
codegen (ELet x t1 t2) = do
    p <- isBounded x
    if p
    then do
        y <- genNewVar x
        codegen (ELet y t1 (rename x y t2))
    else do
        codegen t1
        writeCode $ PopTo x
        codegen t2
codegen (ELit (LInt x)) = writeCode $ PushInt x
codegen (ELit (LReal x)) = writeCode $ PushReal x
codegen (ELit (LBool x)) = writeCode $ PushBool x
codegen (ELit (LFun x _)) = writeCode $ PushFn x
codegen _ = undefined 


codegenTop :: Expr -> [ByteCode]
codegenTop expr =
    let (xs,t) = lamUnfold expr
        stateInit = GenState {
              block = []
            , vars = xs
        }
        (a,s) = runState (codegen t) stateInit
        codelist = reverse $ block s
    in fmap PopTo xs ++ codelist





    







byteGen :: [ByteCode] -> Expr -> Either Name [ByteCode]
byteGen bc (EVar x) = Right (PushVar x: bc)
byteGen _ (ELam _ _) =
    Left "No lambda should appear inside a supercombinator!"
byteGen bc t@(EApp _ _) = do
    let (t1:ts) = appUnfold t
    ts' <- forM ts 
    case t1 of
        EVar t
    
    c2 <- byteGen [] e2
    c1 <- byteGen [] e1
    Right $ c2 ++ c1 ++ bc
byteGen bc (ELet x e1 e2) = do
    c1 <- byteGen [] e1
    c2 <- byteGen [] e2
    Right $ c2 ++ [PopTo x] ++ c1 ++ bc
byteGen _ _  = undefined 
-}