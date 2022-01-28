{-# LANGUAGE OverloadedStrings #-}

module TypeInfer where
import Utils

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

data InferError =
      OccurCheckFailed
    | TupleDiffLength
    | ConstUnifyFailed
    | UnboundVariable
    deriving (Show)

instance Pretty InferError where
    pretty OccurCheckFailed = "Occur Check Failed!"
    pretty TupleDiffLength = "Tuple of Different Length!"
    pretty ConstUnifyFailed = "Constant Unify Failed!"
    pretty UnboundVariable = "Unbound Variable!"

type Logs = Doc T.Text

newtype TypeEnv = TypeEnv { getEnv :: M.Map Name Type }

type Infer a =  ExceptT InferError (RWS TypeEnv Logs Integer) a


type Subst = M.Map Name Type

newVar :: Infer Name
newVar = do
    n <- get
    put (n + 1)
    return $ T.pack ("#" ++ show n)

nestedLog :: Infer a -> Infer a
nestedLog = censor (\log -> hardline <> indent 2 log <> hardline)


logEnv :: Infer ()
logEnv = do
    env <- ask
    tell (pretty $ "env: " ++ show (getEnv env))

mapEnv :: (M.Map Name Type -> M.Map Name Type) -> TypeEnv -> TypeEnv
mapEnv f = TypeEnv . f . getEnv

updateEnv :: Name -> Type -> TypeEnv -> TypeEnv
updateEnv x ty = mapEnv (M.insert x ty)

lookupEnv :: Name -> Infer (Maybe Type)
lookupEnv x = do
    (TypeEnv env) <- ask
    return $ M.lookup x env


runInfer :: Infer a -> (Either InferError a,Logs)
runInfer ti = evalRWS (runExceptT ti) (TypeEnv M.empty) 0

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
    ftv l = foldr (S.union . ftv) S.empty l

instance Types TypeEnv where
    ftv (TypeEnv env) = ftv (M.elems env)
    apply s (TypeEnv env) = TypeEnv (M.map (apply s) env)


generalize :: TypeEnv -> Type -> Type
generalize env t =
    let vars = S.toList (ftv t `S.difference` ftv env)
    in if null vars then t else TForall vars t

instantiate :: [Name] -> Type -> Infer Type
instantiate vars t = do
    nvars <- mapM (const newVar) vars
    let s = M.fromList (zip vars (fmap TVar nvars))
    return $ apply s t

unify :: Type -> Type -> Either InferError Subst
unify (TArr a1 b1) (TArr a2 b2) = do
    s1 <- unify a1 a2
    s2 <- unify (apply s1 b1) (apply s1 b2)
    Right $ s2 `compose` s1
unify (TVar a) (TVar b) = Right $
    if a == b then M.empty
    else M.singleton a (TVar b)
unify (TVar a) t = if a `S.member` ftv t
    then Left OccurCheckFailed
    else Right $ M.singleton a t
unify t (TVar a) = unify (TVar a) t
unify (TTup []) (TTup []) = Right M.empty
unify (TTup []) (TTup _) = Left TupleDiffLength
unify (TTup _) (TTup []) = Left TupleDiffLength
unify (TTup (x:xs)) (TTup (y:ys)) = do
    s1 <- unify x y
    s2 <- unify (TTup (apply s1 xs)) (TTup (apply s1 ys))
    Right $ s1 `compose` s2
unify TInt TInt = Right M.empty
unify TBool TBool = Right M.empty
unify TReal TReal = Right M.empty
unify t1 t2 = Left ConstUnifyFailed

tiLiteral ::  Literal -> (Subst, Type)
tiLiteral (LInt _) = (M.empty, TInt)
tiLiteral (LReal _) = (M.empty, TReal)
tiLiteral (LBool _) = (M.empty, TBool)
tiLiteral (LFun _ t) = (M.empty, t)

ti :: Expr -> Infer (Subst, Type)
ti (EVar x) = do
    mayTy <- lookupEnv x
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
    tv <- TVar <$> newVar
    tell $ "+ Assume that variable" <+> pretty x
        <+> "has type" <+> pretty tv
    (s1 , t1) <- nestedLog $ local (updateEnv x tv) (ti e)
    tell $ "- So expression" <+> pretty e 
        <+> "has type: " <+> pretty t1
    return (s1 , TArr (apply s1 tv) t1)
ti (EApp e1 e2) = do
    tell $ "+ In application " <+> pretty (EApp e1 e2)
    (s1, t1) <- nestedLog $ ti e1
    tell $ "-" <+> pretty e1 <+> "has type: " <+> pretty t1
    (s2, t2) <- nestedLog $ local (apply s1) (ti e2)
    tell $ "-" <+> pretty e2 <+> "has type: " <+> pretty t2
    tv <- TVar <$> newVar
    case unify (apply s2 t1) (TArr t2 tv) of
        Left err -> throwError err
        Right s3 -> do
            let s' = s1 `compose` s2 `compose` s3
            let t' = apply s3 tv
            tell $ hardline <> "- unify them:" <+> viaShow (show s')
            tell $ pretty t'
            return (s',t')
            
ti (ELet x e1 e2) = do
    env <- ask
    (s1, t1) <- ti e1
    let t1' = generalize (apply s1 env) t1
    (s2, t2) <- local (apply s1 . updateEnv x t1') (ti e2)
    return (s1 `compose` s2, t2)
ti (ELit lit) = return $ tiLiteral lit
ti _ = return (M.empty , TInt)

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