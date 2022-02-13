{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module TypeInfer where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Bifunctor as Bi

import Debug.Trace
import Control.Monad.Except
import Control.Monad.RWS
import Control.Exception (throw)
import Prettyprinter

import Utils
import Environment
import Control.Monad.Reader (runReader)

data InferError =
      OccurCheckFailed
    | TupleDiffLength
    | ConstUnifyFailed
    | UnboundVariable
    | NotAConstructor
    | PatternNotExhausted
    | CaseConstructorNotMatch
    | UnknownError

instance Pretty InferError where
    pretty OccurCheckFailed = "Occur Check Failed!"
    pretty TupleDiffLength = "Tuple of Different Length!"
    pretty ConstUnifyFailed = "Constant Unify Failed!"
    pretty UnboundVariable = "Unbounded Variable!"
    pretty NotAConstructor = "Not a Constructor!"
    pretty PatternNotExhausted = "Pattern Not Exhausted!"
    pretty CaseConstructorNotMatch = "Branch Constructor not matched!"
    pretty UnknownError = "Unknown Error!"

instance Show InferError where
    show e = T.unpack $ docRender (pretty e)

type Infer a =  ExceptT InferError Env a

type Subst = M.Map Name Type

runInfer :: Infer a -> Env (Either InferError a)
runInfer = runExceptT


compose :: Subst -> Subst -> Subst
compose s1 s2 = M.map (apply s2) s1 `M.union` s2

class Types a where
    ftv :: a -> S.Set Name
    apply :: Subst -> a -> a

instance Types Type where
    ftv (TVar n) = S.singleton n
    ftv (TArr t1 t2) = ftv t1 `S.union` ftv t2
    ftv _ = S.empty

    apply s (TVar n) =
        case M.lookup n s of
            Nothing -> TVar n
            Just t -> t
    apply s (TArr t1 t2) = TArr (apply s t1) (apply s t2 )
    apply s other = other

instance Types a => Types [a] where
    apply s = fmap (apply s)
    ftv = foldr (S.union . ftv) S.empty

instance Types EnvReader where
    ftv (EnvReader env) = ftv (M.elems env)
    apply s (EnvReader  env) = EnvReader (M.map (apply s) env)

generalize :: (MonadReader EnvReader m) => Type -> m Type
generalize t = do
    env <- ask
    let vars = S.toList $ ftv t `S.difference` ftv env
    return $ if null vars then t else TForall vars t

instantiate :: (MonadState EnvState m) => [Name] -> Type -> m Type
instantiate vars t = do
    nvars <- mapM (const newSymb) vars
    let s = M.fromList (zip vars (fmap TVar nvars))
    return $ apply s t

{-
tupleUnify :: [Type] -> [Type] -> Infer Subst
tupleUnify [] [] = return M.empty
tupleUnify xs [] = throwError TupleDiffLength
tupleUnify [] ys = throwError TupleDiffLength
tupleUnify (x:xs) (y:ys) = do
    s1 <- unify x y
    s2 <- tupleUnify (apply s1 xs) (apply s1 ys)
    return $ s1 `compose` s2
-}

unify :: Type -> Type -> Infer Subst
unify (TVar a) (TVar b) = return $
    if a == b then M.empty
    else M.singleton a (TVar b)
unify (TVar a) t = if a `S.member` ftv t
    then throwError OccurCheckFailed
    else return $ M.singleton a t
unify t (TVar a) = unify (TVar a) t
unify (TCon a) (TCon b) =
    -----------------
    undefined --todo
    -----------------
unify (TArr a1 b1) (TArr a2 b2) = do
    s1 <- unify a1 a2
    s2 <- unify (apply s1 b1) (apply s1 b2)
    return $ s2 `compose` s1

unify (TTup []) (TTup []) = return M.empty
unify (TTup xs) (TTup ys)
    | length xs == length ys = 
        let f s0 (x,y) = do
                s1 <- unify (apply s0 x) (apply s0 y)
                return (s0 `compose` s1)
        in foldM f M.empty (zip xs ys) 
    | otherwise = throwError TupleDiffLength

unify (TLit l1) (TLit l2)
    | l1 == l2 = return M.empty
    | otherwise = throwError ConstUnifyFailed
unify t1 t2 = throwError UnknownError


ti :: Expr -> Infer (Subst, Type)
ti (EVar x) = do
    mayTy <- asksTypeEnv (M.lookup x)
    case mayTy of
        Nothing -> throwError UnboundVariable
        Just (TForall xs ty) -> do
            t <- instantiate xs ty
            tell $ pretty x <+> "has type:" <+> pretty ty <+> softline
                <+> ", It was instantiated into" <+> pretty t
            return (M.empty, t)
        Just ty -> do
            tell $ pretty x <+> "has type:" <+> pretty ty
            return (M.empty, ty)
ti (ELam x e) = do
    tv <- TVar <$> newSymb
    tell $ "+ Assume that variable" <+> pretty x <+> "has type" <+> pretty tv
    (s1 , t1) <- nestedLog $ localTypeEnv (M.insert x tv) (ti e)
    tell $ "- So expression" <+> pretty e <+> "has type: " <+> pretty t1
    return (s1 , TArr (apply s1 tv) t1)
ti (EApp e1 e2) = do
    tell $ "+ In application" <+> pretty (EApp e1 e2)
    (s1, t1) <- nestedLog $ ti e1
    tell $ "-" <+> pretty e1 <+> "has type: " <+> pretty t1
    (s2, t2) <- nestedLog $ local (apply s1) (ti e2)
    tell $ "-" <+> pretty e2 <+> "has type: " <+> pretty t2
    tv <- TVar <$> newSymb
    s3 <- unify (apply s2 t1) (TArr t2 tv)
    let s' = s1 `compose` s2 `compose` s3
    let t' = apply s3 tv
    tell $ hardline <> "- unify them:" <+> viaShow (show s')
    tell $ pretty t'
    return (s',t')

ti (ELet x e1 e2) = do
    (s1, t1) <- ti e1
    local (apply s1) $ do
        t1' <- generalize t1
        (s2, t2) <- localTypeEnv (M.insert x t1') (ti e2)
        return (s1 `compose` s2, t2)

ti (ETup []) = return (M.empty, TLit TUnit)
ti (ETup xs) =
    let f (s0, ts) x = do
            (s1, t) <- local (apply s0) (ti x)
            return (s0 `compose` s1, t:ts)
    in Bi.second TTup <$> foldM f (M.empty, []) xs

ti (ELit lit) = return $ tiLiteral lit

ti (EIfte c t f) = do
    (s1, tc) <- ti c
    s2 <- unify (apply s1 tc) (TLit TBool)
    local (apply $ s1 `compose` s2) $ do
        (s3, tt) <- ti t
        (s4, tf) <- local (apply s3) (ti f)
        s5 <- unify (apply (s3 `compose` s4) tt) (apply s4 tf)
        let bigS = foldl compose M.empty [s1,s2,s3,s4,s5]
        return (bigS,apply bigS tt)

ti _ = return (M.empty , TLit TInt)

ti (ECase x css) = do
    tell $ "+ In Pattern Matching:" <+> pretty x
    (s1, ty) <- ti x




{-

    (s1, t1) <- nestedLog $ ti x
    case unify t1 (TVar ty) of


    let cons =  . dt_branchs) 


    mapM (a -> m b) (t a)

    where
        caseTi Branch{..} = do
            case getData con of
                Just DataDecl{name,args,branches} -> do
                    let len = length args

                
                Nothing -> throwError e
-}

tiPattern :: Type -> Pattern -> Infer (Subst, Type) -> Infer (Subst, Type)
tiPattern ty (PVar x) = localTypeEnv (M.insert x ty)
--tiPattern ty (PCon x xs) = do
    --dt <- getData
    --case dt of
        --Just (DataDecl {y,ys))
tiPattern ty ()



tiLiteral ::  LitValue -> (Subst, Type)
tiLiteral (LInt _) = (M.empty, TLit TInt)
tiLiteral (LReal _) = (M.empty, TLit TReal)
tiLiteral (LBool _) = (M.empty, TLit TBool)

{-
typeInfer :: M.Map Name Type -> Expr -> Infer Type
typeInfer env e = do
    (s, t) <- local (const $ TypeEnv env) (ti e)
    return (apply s t)

typeInferTop :: Expr -> (Either InferError Type, Logs)
typeInferTop expr = runInfer $ typeInfer M.empty expr


typeInferIO :: Expr -> IO ()
typeInferIO expr =
    let (ty,log) = typeInferTop expr
    in case ty of
        Left err -> do
            putStrLn "Fatal: An error occured in type inference!"
            print err
        Right ty -> do
            putStrLn "type inference successed!"
            print ty


showResult :: (Either InferError Type, Logs) -> T.Text 
showResult (Left err, logs) =
    T.pack $ "Type Inference Error:" ++ show err ++ "\n" ++ show logs
showResult (Right ty, logs) =
    T.pack $ "Type:" ++ show ty ++ "\n" ++ show logs 

inferTest :: String
inferTest = do
    let s1 = M.fromAscList [("#0",TArr (TVar "#2") (TVar "#3"))]
    let s2 = M.fromAscList [("#3",TArr (TVar "#1") (TVar "#4"))]
    show $ s1 `compose` s2

{-
f :: t1
g :: t2
x :: t3

f x g :: t3 -> t4
f x :: t2 -> t3 -> t4
f :: t1 -> t2 -> t3 -> t4



-}

-}