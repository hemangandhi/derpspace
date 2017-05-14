-- import Data.Maybe

-- TODO: Generic tree
-- Generic wrapper for types with size.

-- Edge is a stand-in for null
data BST a = Edge
           | BST a (BST a) (BST a)
           deriving (Show)

instance Functor BST where
    fmap f Edge        = Edge
    fmap f (BST v l r) = BST (f v) (fmap f l) (fmap f r)

instance Foldable BST where
    foldMap f Edge        = mempty
    foldMap f (BST v l r) = foldMap f l `mappend` f v `mappend` foldMap f r

insert :: (Ord a) => BST a -> a -> BST a
insert Edge val      = BST val Edge Edge
insert (BST v l r) i
       | i == v      = Edge
       | i <  v      = BST v (insert l i) r
       | i >  v      = BST v l (insert r i)

insert_list :: (Ord a) => [a] -> BST a
insert_list = foldr (flip insert) Edge

to_list :: (BST a) -> [a]
to_list Edge        = []
to_list (BST v l r) = to_list l ++ [v] ++ to_list r

tree_sort :: (Ord a) => [a] -> [a]
tree_sort = to_list . insert_list

has_elt :: (Ord a) => (BST a) -> a -> Bool
has_elt Edge _        = False
has_elt (BST v l r) k
        | v == k      = True
        | v <  k      = has_elt k l
        | v >  k      = has_elt k r
