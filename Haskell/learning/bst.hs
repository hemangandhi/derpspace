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
        | v >  k      = has_elt l k
        | v <  k      = has_elt r k

delete_node :: (Ord a) => (BST a) -> a -> BST a
delete_node Edge _           = Edge
delete_node (BST v Edge r) i
            | v == i         = r
            | v >  i         = Edge
            | v <  i         = BST v Edge $ delete_node r i
delete_node (BST v l Edge) i
            | v == i         = l
            | v >  i         = BST v (delete_node l i) Edge
            | v <  i         = Edge
delete_node (BST v l r) i
            | v <  i         = BST v l (delete_node r i)
            | v >  i         = BST v (delete_node l i) r
            | v == i         = BST (fst iop) (snd iop) r
                             where find_iop (BST v_i l_i Edge) = (v_i, l_i)
                                   find_iop (BST v_i l_i r_i)  = (fst rec, BST v_i l_i (snd rec))
                                                               where rec = find_iop r_i
                                   iop = find_iop l
