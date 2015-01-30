data BST k v = Empty |
               Node k v (BST k v) (BST k v)

val :: BST k v -> Maybe v
val tree key value = value