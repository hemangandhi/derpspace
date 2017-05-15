import Data.Maybe

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

to_list :: (BST a) -> [a]
to_list Edge        = []
to_list (BST v l r) = to_list l ++ [v] ++ to_list r

--Before this, BST is just a regular binary tree.
--Now comes the searching bit...

insert :: (Ord a) => BST a -> a -> Maybe (BST a)
insert Edge val      = Just $ BST val Edge Edge
insert (BST v l r) i
       | i == v      = Nothing
       | i <  v      = case (insert l i) of
                       Nothing   -> Nothing
                       Just nl   -> Just $ BST v nl r
       | i >  v      = case (insert r i) of
                       Nothing   -> Nothing
                       Just nr   -> Just $ BST v l nr

insert_list :: (Ord a) => [a] -> Maybe (BST a)
insert_list = foldr (\x n -> case n of Nothing -> Nothing
                                       Just v  -> insert v x) (Just Edge)

tree_sort :: (Ord a) => [a] -> Maybe [a]
tree_sort = (\mbfs -> case mbfs of Just b  -> Just $ to_list b
                                   Nothing -> Nothing) . insert_list

has_elt :: (Ord a) => (BST a) -> a -> Bool
has_elt Edge _        = False
has_elt (BST v l r) k
        | v == k      = True
        | v >  k      = has_elt l k
        | v <  k      = has_elt r k

delete_node :: (Ord a) => (BST a) -> a -> Maybe (BST a)
delete_node Edge _           = Nothing
delete_node (BST v Edge r) i
            | v == i         = Just r
            | v >  i         = Just Edge
            | v <  i         = case delete_node r i of
                               Just nr -> Just $ BST v Edge nr
                               Nothing -> Nothing
delete_node (BST v l Edge) i
            | v == i         = Just l
            | v >  i         = case delete_node l i of
                               Just nl -> Just $ BST v nl Edge
                               Nothing -> Nothing
            | v <  i         = Nothing
delete_node (BST v l r) i
            | v <  i         = case (delete_node r i) of
                               Just nr -> Just $ BST v l nr
                               Nothing -> Nothing
            | v >  i         = case (delete_node l i) of
                               Just nl -> Just $ BST v nl r
                               Nothing -> Nothing
            | v == i         = Just $ BST (fst iop) (snd iop) r
                             where find_iop (BST v_i l_i Edge) = (v_i, l_i)
                                   find_iop (BST v_i l_i r_i)  = (fst rec, BST v_i l_i (snd rec))
                                                               where rec = find_iop r_i
                                   iop = find_iop l
