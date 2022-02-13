{-# LANGUAGE FlexibleContexts #-}

module Environment where
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Text as T
import Control.Monad.Except
import Control.Monad.RWS
import Prettyprinter

import Utils

data FuncDecl = FuncDecl {
      fn_name :: Name
    , fn_def :: Expr
}

data TypeDecl = TypeDecl {
      ty_name :: Name
    , ty_args :: [Name]
    , ty_def :: Type
}

data DataDecl = DataDecl {
      dt_con :: Name
    , dt_args :: [Name]
    , dt_branchs :: [(Name,[Type])]
}

data Decl
    = DFunc FuncDecl
    | DType TypeDecl
    | DData DataDecl

type Env = RWS EnvReader EnvWriter EnvState

data EnvReader = EnvReader
    { _typeEnv ::M.Map Name Type
    }

type EnvWriter = Doc T.Text

data EnvState = EnvState
    { _modName :: Name
    , _subMod :: M.Map Name EnvReader
    , _dataDefs :: M.Map Name DataDecl
    , _typeDefs :: M.Map Name TypeDecl
    , _funcDefs :: M.Map Name FuncDecl
    , _gensym :: Integer
    -- , _flags :: CompilerFlags
    }

{-
data CompilerFlags = CompilerFlags {
    _flag_debug_level :: Int
}
-}

bounded :: Env (S.Set Name)
bounded = do
    dt <- gets (M.keys . _dataDefs)
    ty <- gets (M.keys . _typeDefs)
    fn <- gets (M.keys . _funcDefs)
    return (S.fromList $ dt ++ ty ++ fn)

--isFree :: Name -> Env Bool

-- lookupEnv :: (MonadReader EnvReader m) => Name -> m (Maybe Type)
-- lookupEnv x = asks (M.lookup x . _typeEnv)

asksTypeEnv :: (MonadReader EnvReader m) => (M.Map Name Type -> a) -> m a
asksTypeEnv f = asks (f . _typeEnv)

localTypeEnv :: (MonadReader EnvReader m) => (M.Map Name Type -> M.Map Name Type) -> m a -> m a
localTypeEnv f = local (EnvReader . f . _typeEnv)


log :: (MonadWriter EnvWriter m) => EnvWriter -> m ()
log = tell

nestedLog :: (MonadWriter EnvWriter m) => m a -> m a
nestedLog = censor (\log -> hardline <> indent 2 log <> hardline)

newSymb :: (MonadState EnvState m) => m Name
newSymb = do
    n <- gets _gensym
    return $ T.pack ("#" ++ show n)

getData :: (MonadState EnvState m) => Name -> m (Maybe DataDecl)
getData x = gets (M.lookup x . _dataDefs)

getType :: (MonadState EnvState m) => Name -> m (Maybe TypeDecl)
getType x = gets (M.lookup x . _typeDefs)

getFunc :: (MonadState EnvState m) => Name -> m (Maybe FuncDecl)
getFunc x = gets (M.lookup x . _funcDefs)


getConstructor :: Name -> Maybe [Type]

emptyEnvReader = EnvReader {
      _typeEnv = M.empty
}

emptyEnvState = EnvState {
      _modName = T.pack "empty"
    , _subMod = M.empty
    , _dataDefs = M.empty
    , _typeDefs = M.empty
    , _funcDefs = M.empty
    , _gensym = 0
}


runEnv :: Env a -> (a, EnvWriter)
runEnv env = evalRWS env emptyEnvReader emptyEnvState 

