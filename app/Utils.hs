{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Utils where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe

import qualified Data.Text as T
import Prettyprinter as P
    ( (<+>),
      defaultLayoutOptions,
      encloseSep,
      hardline,
      layoutPretty,
      sep,
      softline,
      tupled,
      vsep,
      Doc,
      Pretty(pretty) )
import Prettyprinter.Render.Text as PRT


type Name = T.Text

data Expr =
      EVar Name
    | ELam Name Expr
    | EApp Expr Expr
    | ELet Name Expr Expr
    | ETup [Expr] -- (Expr, ... , Expr)
    | ESum Int Int Expr -- <_, ... , Expr, ... ,_>
    | ELit LitValue
    | EIfte Expr Expr Expr
    | EMatch Expr [Name] Expr
    | ECase Expr [(Name,Expr)]
    | EAnno Expr Type
    deriving (Eq, Ord)

-- | EBlock (M.Map Name Expr) Expr

data Pattern =
      PVar Name
    | PCon Name [Name]
    | PTup [Pattern]
    | PLit LitValue
    | PWild
    deriving (Eq, Ord)

data Type =
      TVar Name
    | TLam Name Kind Type
    | TApp Type Type
    | TLit LitType
    | TArr Type Type
    | TTup [Type]
    | TSum [Type]
    | TForall [Name] Type
    deriving (Eq, Ord)

data Kind = 
      Star
    | KArr Kind Kind
    deriving (Eq, Ord)

data LitValue =
      LInt Int
    | LReal Double
    | LBool Bool
    deriving (Eq, Ord)

data LitType =
      TInt
    | TReal
    | TBool
    | TUnit
    | TVoid
    deriving (Eq, Ord)

bracketed :: [Doc ann] -> Doc ann
bracketed =  P.encloseSep "(" ")" " "

docRender :: Doc ann -> T.Text
docRender = renderStrict . layoutPretty defaultLayoutOptions

instance Pretty Kind where
    pretty Star = "*"
    pretty (KArr k1 k2) = pretty k1 <+> "->" <+> pretty k2

instance Pretty Type where
    pretty (TVar x) = pretty x
    pretty (TLam x k t) =
        "(Λ." <+> pretty x <+> ":" <+> pretty k <+> "->" <+> pretty t <> ")"
        -- todo : fold and unfold for TLam
    pretty (TApp t1 t2) = "(" <+> pretty t1 <+> pretty t2 <+> ")"
    pretty (TLit lit) = pretty lit
    pretty t@TArr{} = P.encloseSep "(" ")" " -> "
        (fmap pretty (tyArrUnfold t))
    pretty (TTup xs) = P.encloseSep "(" ")" "," (fmap pretty xs)
    pretty (TSum xs) = P.encloseSep "<" ">" "," (fmap pretty xs)
    pretty (TForall xs ty) =
        "forall" <+> P.sep (fmap pretty xs) <+> pretty ty

instance Pretty LitValue where
    pretty (LInt n) = pretty n
    pretty (LReal x) = pretty x
    pretty (LBool p) = if p then "true" else "false"

instance Pretty LitType where
    pretty TInt = "Int"
    pretty TReal = "Real"
    pretty TBool = "Bool"
    pretty TUnit = "Unit"
    pretty TVoid = "Void"

instance Pretty Expr where
    pretty (EVar x) = pretty x
    pretty t@ELam{} =
        "(λ." <+> P.sep xs <+> "->" <+> P.sep ys <> ")"
        where
            (xs',t') = lamUnfold t
            xs = fmap pretty xs'
            ys = fmap pretty (appUnfold t')

    pretty t@EApp{} =
        P.encloseSep "(" ")" " " xs
        where xs = fmap pretty (appUnfold t)
    pretty t@ELet{} =
        "let" <+> P.vsep xs <+> P.softline <+> "in" <+> pretty t
        where
            (xs',t') = letUnfold t
            xs = fmap (\(x,y) -> pretty x <+> "=" <+> pretty y) xs'
        
    pretty (ELit b) = pretty b
    pretty (ETup xs) = P.encloseSep "(" ")" "," (fmap pretty xs)
    pretty (ESum i j expr) = P.encloseSep "<" ">" "," (xs ++ [pretty expr] ++ ys) 
        where
            (xs,_:ys) = splitAt j (replicate i "_")
    pretty (EIfte cd tr fl) = P.sep
        [ "if" <+> pretty cd
        , "then" <+> pretty tr
        , "else" <+> pretty fl
        ]
    pretty (EMatch expr xs t) = 
        "match" <+> pretty expr <+> "with" <+>
            P.sep (fmap pretty xs) <+> "=>" <> hardline <> pretty t
    pretty (ECase expr cases) =
        "case" <+> pretty expr <+> "of" <> hardline
            <> P.vsep (fmap prettyBranch cases)
            where
                prettyBranch (pat,body) =
                    "|" <+> pretty pat <+> "=>" <+> pretty body
    
    pretty (EAnno expr ty) = pretty expr <+> ":" <+> pretty ty

instance Pretty Pattern where
    pretty (PVar x) = pretty x
    pretty (PCon x xs) = P.encloseSep "(" ")" " "
                        (pretty x: fmap pretty xs)
    pretty (PTup xs) = tupled (fmap pretty xs)
    pretty (PLit lit) = pretty lit
    pretty PWild = "_"

tyArrFold :: [Type] -> Type
tyArrFold = foldr1 TArr

tyArrUnfold :: Type -> [Type]
tyArrUnfold t = tyArrUnfold' t []
    where
        tyArrUnfold' (TArr t1 t2) xs = tyArrUnfold' t2 (t1:xs)
        tyArrUnfold' other xs = reverse (other:xs)

tyAppFold :: [Type] -> Type
tyAppFold = foldl1 TApp

tyAppUnfold :: Type -> [Type]
tyAppUnfold t = tyAppUnfold' t []
    where
        tyAppUnfold' (TApp t1 t2) xs = tyAppUnfold' t1 (t2:xs)
        tyAppUnfold' other xs = other:xs


lamFold :: ([Name],Expr) -> Expr
lamFold (xs,t) = foldr ELam t xs

lamUnfold :: Expr -> ([Name],Expr)
lamUnfold t = lamUnfold' t []
    where
        lamUnfold' (ELam x t) xs = lamUnfold' t (x:xs)
        lamUnfold' other xs = (reverse xs,other)

appFold :: [Expr] -> Expr
appFold = foldl1 EApp

appUnfold :: Expr -> [Expr]
appUnfold t = appUnfold' t []
    where
        appUnfold' (EApp t1 t2) xs = appUnfold' t1 (t2:xs)
        appUnfold' other xs = other:xs

letFold :: ([(Name,Expr)],Expr) -> Expr
letFold (xs,expr) = foldr (uncurry ELet) expr xs

letUnfold :: Expr -> ([(Name,Expr)],Expr)
letUnfold t = letUnfold' t []
    where
        letUnfold' (ELet x t1 t2) xs = letUnfold' t2 ((x,t1):xs)
        letUnfold' other xs = (xs,other)

freeVar :: Expr -> [Name]
freeVar (EVar x) = [x]
freeVar (ELam x t) = filter (/= x) (freeVar t)
freeVar (EApp t1 t2) = freeVar t1 ++ freeVar t2
freeVar _ = []

isFree :: Name -> Expr -> Bool
isFree v (EVar x) = x /= v
isFree v (ELam x t) = x == v || isFree v t
isFree v (EApp t1 t2) = isFree v t1 && isFree v t2
isFree v atom = True

rename :: Name -> Name -> Expr -> Expr
rename a b (EVar x) =
    if x == a then EVar b else EVar x
rename a b t0@(ELam x t) =
    if x == a then t0 else ELam x (rename a b t)
rename a b (EApp t1 t2) = EApp (rename a b t1) (rename a b t2)
rename a b t0@(ELet x t1 t2) =
    if x == a then t0 else ELet x (rename a b t1) (rename a b t2)
rename _ _ other = other


class Substitutable a where
    free :: a -> [Name]
    subst :: [(Name,a)] -> a -> a

instance Substitutable Type where
    free (TVar x) = [x]
    free (TArr t1 t2) = free t1 ++ free t2
    free (TTup xs) = concatMap free xs
    free (TForall xs ty) = filter (`notElem` xs) (free ty)
    free _ = []

    subst s (TVar n) = fromMaybe (TVar n) (L.lookup n s)
    subst s (TArr t1 t2) = TArr (subst s t1) (subst s t2)
    subst s (TTup xs) = TTup (fmap (subst s) xs)
    subst s (TForall xs ty) =
        let s' = filter (\(k,v) -> k `notElem` xs) s
        in TForall xs (subst s' ty)
    subst s other = other

instance Show Type where
    show e = T.unpack $ docRender (pretty e)

instance Show Expr where
    show e = T.unpack $ docRender (pretty e)

instance Show LitValue where
    show e = T.unpack $ docRender (pretty e)

instance Show Pattern where
    show e = T.unpack $ docRender (pretty e)