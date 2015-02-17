--Ryan Schreiber
--Partner Fedor Nikolay

import Data.List

class Json a where
  toJson :: a -> JSON
  fromJson :: JSON -> a


--Problem1
data BST k v = Empty |
               Node k v (BST k v) (BST k v)

val :: BST k v -> Maybe v
val Empty = Nothing
val (Node k v _ _) = Just v

--Problem2
size :: BST k v -> Int
size Empty = 0
size (Node _ _ a b) = size a + size b + 1

--Problem3
ins :: (Ord k) => k -> v -> BST k v -> BST k v
ins k v Empty = (Node k v Empty Empty)
ins k v (Node key value left right) 
	| k<key = Node key value (ins k v left) right
	| k>key = Node key value left (ins k v right)
	| otherwise = Node k v left right

--Problem4
instance (Show v) => Show (BST k v) where
  show Empty = ""
  show (Node k v left right) = "("++show left ++ show v ++ show right++")"

--Problem5
data JSON = JStr String
          | JNum Double
          | JArr [JSON]
          | JObj [(String, JSON)]

instance Show JSON where
  show (JStr s) = show s
  show (JNum n) = show n
  show (JArr a) = "["++intercalate "," (map show a)++"]"
  --couldn't find way to add JSON portion of obj
  show (JObj o) = "{"++intercalate "," (map show((map (++":") (map fst o))))++"}"

--Problem6
instance Json Double where
    toJson d = JNum d
    fromJson (JNum d) = d

instance (Json a) => Json [a] where 
    toJson xs = JArr (map toJson xs)
    fromJson (JArr xs) = map (fromJson) xs

