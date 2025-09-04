module ListMap (Map, keys, values, find, insert) where

import Variable

type Map k v = [(k, (Maybe v))]

keys :: Map k v -> [k]
keys = map fst

values :: Map k v -> [Maybe v]
values = map snd

maybeVar :: Maybe a -> Variable a
maybeVar Nothing = Declared
maybeVar (Just v) = Defined v

find :: (Eq k) => Map k v -> k -> Variable v
find [] _ = Undeclared
find ((key, val):ts) k
    | k == key = maybeVar val
    | otherwise = find ts k

match :: (Eq a) => (a, b) -> (a, c) -> Bool
match (x, _) (y, _) = x == y

insert :: (Eq k) => Map k v -> (k, Maybe v) -> Map k v
insert = helper [] where
    helper :: (Eq k) => Map k v -> Map k v -> (k, Maybe v) -> Map k v
    helper rest [] a = a:(reverse rest)
    helper rest (t:ts) a
        | match t a = reverse rest ++ (a:ts)
        | otherwise = helper (t:rest) ts a