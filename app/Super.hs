module Super where

import Control.Monad.State.Lazy
import Data.List as L
import Data.Map as M
import Data.Set as S
import Data.Text as T
import Utils

data SuperState = SuperState
    { fresh :: Int
    , defs  :: M.Map Name Expr
    }

initSuperState :: SuperState
initSuperState = SuperState
    { fresh = 0
    , defs = M.empty
    }

type SuperCmpl a = State SuperState a

freshVar :: SuperCmpl Name
freshVar = do
    s <- get
    let i = fresh s
    put s{fresh = i + 1}
    return $ T.pack ("#" ++ show i)

newDef :: Expr -> SuperCmpl Name
newDef expr = do
    name <- freshVar
    modify (\s -> s {defs = M.insert name expr (defs s)})
    return name

super :: Expr -> SuperCmpl (Expr,S.Set Name)
super t@(EVar x) = if T.head x == '#'
    then return (t,S.empty)
    else return (t,S.singleton x)
super t0@(ELam _ _) =
    let (xs,t1) = lamUnfold t0
    in do
        (t2,fv) <- super t1
        let fv' = S.difference fv (S.fromList xs)
        let ys = S.toList fv'
        new <- newDef $ lamFold (ys ++ xs, t2)
        return (appFold (EVar new: fmap EVar ys) , fv')
super t@(EApp t1 t2) = do
    (t1',fv1) <- super t1
    (t2',fv2) <- super t2
    return (EApp t1' t2', fv1 `S.union` fv2)
super t0@(ELet x t1 t2) = do
    (t1',fv1) <- super t1
    (t2',fv2) <- super t2
    return (ELet x t1' t2', (fv1 `S.union` fv2) `S.difference` S.singleton x)


{-
    let (xs,t1) = letUnfold t0
    in do
        xs' <- forM xs (\(x,y) -> do
            (y',_) <- super y
            return (x,y'))
        (t2,fv) <- super t1
        let fv' = S.difference fv (S.fromList (fmap fst xs))
        return (letFold (xs',t2),fv')
-}

super other = return (other,S.empty)


superTop :: Expr -> (Expr,M.Map Name Expr)
superTop expr =
    let ((e,fv),s) = runState (super expr) initSuperState
    in (e,defs s)


lamLift :: Expr -> Expr
lamLift t@(ELam _ _) =
    let vars = freeVar t
        (xs,t') = lamUnfold t
        args = fmap EVar vars
    in appFold (lamFold (vars ++ xs, t') : args)
lamLift _ = error "Lambda Lifting works only for ELam!"