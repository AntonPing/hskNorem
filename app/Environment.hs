module Environment where
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Text as T
import Control.Monad.Except
import Control.Monad.Reader


import Utils

data FuncDecl = FuncDecl {
      fn_name :: Name
    -- , fn_args :: [Name]
    , fn_def :: Expr
}

data TypeDecl = TypeDecl {
      ty_name :: Name
    , ty_args :: [Name]
    , ty_def :: Type
}

data DataDecl = DataDecl {
      dt_cons :: Name
    , dt_args :: [Name]
    , dt_branchs :: [(Name,[Type])]
}

data Decl
    = DFunc FuncDecl
    | DType TypeDecl
    | DData DataDecl

data EnvLog = EnvLog [String]

type Env = Reader EnvState

data CompilerFlags = CompilerFlags {
    _flag_debug_level :: Int
}

data EnvState = EnvState
    { _modName :: Name
    , _subMod :: M.Map Name EnvState
    , _dataEnv :: M.Map Name DataDecl
    , _typeEnv :: M.Map Name TypeDecl
    , _funcEnv :: M.Map Name FuncDecl
    -- , _flags :: CompilerFlags
    }

getData :: Name -> Env (Maybe DataDecl)
getData x = asks (M.lookup x . _dataEnv)

getType :: Name -> Env (Maybe TypeDecl)
getType x = asks (M.lookup x . _typeEnv)

getFunc :: Name -> Env (Maybe FuncDecl)
getFunc x = asks (M.lookup x . _funcEnv)

emptyEnv :: EnvState
emptyEnv = EnvState
    { _modName = T.pack "empty"
    , _subMod = M.empty
    , _dataEnv = M.empty
    , _typeEnv = M.empty
    , _funcEnv = M.empty
    }