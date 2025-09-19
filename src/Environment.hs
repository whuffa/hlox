module Environment(Environment(..), findRef, insertRef, emptyEnv, fromAscList, fromList) where
    
import Prelude hiding(lookup)
import qualified Data.Map.Strict as TM
import Data.IORef

newtype Environment k v = Environment { getMap :: TM.Map k (IORef v) }


emptyEnv :: Environment k v
emptyEnv = Environment (TM.empty)

findRef :: (Ord k) => Environment k v -> k -> Maybe (IORef v)
findRef (Environment lmap) key = TM.lookup key lmap

insertRef :: (Ord k) => Environment k v -> (k, IORef v) -> Environment k v
insertRef (Environment lmap) (key, val) = Environment (TM.insert key val lmap)

fromAscList :: (Ord k) => [(k, IORef v)] -> Environment k v
fromAscList = Environment . TM.fromAscList

fromList :: (Ord k) => [(k, IORef v)] -> Environment k v
fromList = Environment . TM.fromAscList
