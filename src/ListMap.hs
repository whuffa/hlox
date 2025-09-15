module ListMap (Map, keys, values, find, insert) where

type Map k v = [(k, v)]

keys :: Map k v -> [k]
keys = map fst

values :: Map k v -> [v]
values = map snd

find :: (Eq k) => Map k v -> k -> Maybe v
find [] _ = Nothing
find ((key, val):ts) k
    | k == key = Just val
    | otherwise = find ts k

match :: (Eq a) => (a, b) -> (a, c) -> Bool
match (x, _) (y, _) = x == y

insert :: (Eq k) => Map k v -> (k, v) -> Map k v
insert = helper [] where
    helper :: (Eq k) => Map k v -> Map k v -> (k, v) -> Map k v
    helper rest [] a = a:(reverse rest)
    helper rest (t:ts) a
        | match t a = reverse rest ++ (a:ts)
        | otherwise = helper (t:rest) ts a