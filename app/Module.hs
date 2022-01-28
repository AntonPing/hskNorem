module Module where
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Utils

data Decl
    = DFunc String Expr (Maybe Type)
    | DType String Type
    | DData String [Type] Type


data ModuleState = ModuleState
    { modName :: String
    , subModule :: M.Map String ModuleState
    , dataEnv :: M.Map String Expr
    , nameSupply :: Integer
    }