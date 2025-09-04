module Environment(Environment(..), lookup, declare, define) where

import Prelude hiding(lookup)
import Check
import Variable
import ListMap

data Environment k v
    = Environment { getMap :: Map k v, 
                    parent :: Maybe (Environment k v) }
                    deriving(Show)

lookup :: (Eq k) => Environment k v -> k -> Variable v
lookup env k = helper (Just env) where
    helper Nothing = Undeclared
    helper (Just (Environment lmap p)) = case find lmap k of
        Undeclared -> helper p
        val -> val

declare :: (Eq k) => Environment k v -> k -> Check String (Environment k v)
declare (Environment lmap p) k = case find lmap k of
    Undeclared -> Checked (Environment nmap p) where
        nmap = insert lmap (k, Nothing)
    _ -> Error "Variable already declared in this scope."


define :: (Eq k) => Environment k v -> k -> v -> Check String (Environment k v)
define env key val = let
    assoc = (key, Just val)
    helper Nothing = Error "Cannot define undeclared variable."
    helper (Just (Environment lmap p)) = case find lmap key of
        Undeclared -> helper p
        _ -> Checked (Environment nmap p) where
            nmap = insert lmap assoc
    in helper (Just env)

