
data BST k v = Empty |
               Node k v (BST k v) (BST k v)

--Problem1

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
ins k v (Node key value left right) = 
	if(k<key)
		then ins k v left
	else if(k>key)
		then ins k v right
		else (Node key v left right)

--Problem4
instance (Show v) => Show (BST k v) where
  show Empty = ""
  show (Node k v left right) = "("++show left ++ show v ++ show right++")"
