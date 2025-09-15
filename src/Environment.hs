module Environment(Environment(..), findRef, insertRef) where
    
import Prelude hiding(lookup)
import qualified ListMap as LM
import Data.IORef

--type EnvMap k v = LM.Map k (IORef (Maybe v))
newtype Environment k v = Environment { getMap :: LM.Map k (IORef v) }

findRef :: (Eq k) => Environment k v -> k -> Maybe (IORef v)
findRef (Environment lmap) = LM.find lmap

insertRef :: (Eq k) => Environment k v -> (k, IORef v) -> Environment k v
insertRef (Environment lmap) t = Environment (LM.insert lmap t) 