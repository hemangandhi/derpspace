--Problem 4 (for #10)
myLength :: [a] -> Int
myLength [] = 0
myLength x = tailRec x 0
    where tailRec [] v = v
          tailRec (x:xs) v = tailRec xs (v + 1)

--Problem 5 (for #9)
myReverse :: [a] -> [a]
myReverse arr = tailRec arr []
    where tailRec [] a = a
          tailRec (x:xs) acc = tailRec xs (x:acc)

--Problem 9 (for #10)
pack :: Eq a => [a] -> [[a]]
pack [] = [[]]
pack (x:[]) = [[x]]
pack (x:xs) = tail $ myReverse $ loop xs [x] [[]]
    where loop [] t acc = t:acc
          loop (x:xs) (t:ts) acc
             | x == t = loop xs (x:(t:ts)) acc
             | otherwise = loop xs [x] ((t:ts):acc)

--Problem 10 (for #11)
encode :: Eq a => [a] -> [(Int, a)]
encode arr = map (\pk -> (myLength pk, head pk)) $ pack arr

-- Problem 11
-- (problem 10 used for simplicity)
data Encoding a = Single a | Multiple Int a

tpl_to_enc :: (Int, a) -> Encoding a
tpl_to_enc (1, v) = Single v
tpl_to_enc (n, v) = Multiple n v

encode_mod :: Eq a => [a] -> [Encoding a]
encode_mod arr = map tpl_to_enc $ encode arr

--Let it be printed out...
show_enc :: Show a => Encoding a -> String
show_enc (Single a) = "Single " ++ show a
show_enc (Multiple n a) = show n ++ " of " ++ show a
instance Show a => Show (Encoding a) where
    show t = show_enc t

--Problem 12
decode_mod :: [Encoding a] -> [a]
decode_mod arr = loop arr []
    where loop [] v = v
          loop ((Single t):es) v = loop es $ v ++ [t]
          loop ((Multiple 2 t):es) v = loop es $ v ++ [t, t]
          loop ((Multiple n t):es) v = loop ((Multiple (n-1) t):es) $ v ++ [t]

--Problem 13
encode_dir :: Eq a => [a] -> [Encoding a]
encode_dir arr = myReverse $ loop arr []
    where loop [] acc = acc
          loop (x:xs) [] = loop xs [Single x]
          loop (x:xs) ((Single v):vs)
             | x == v = loop xs ((Multiple 2 v):vs)
             | otherwise = loop xs ((Single x):((Single v):vs))
          loop (x:xs) ((Multiple n v):vs)
             | x == v = loop xs ((Multiple (n+1) v):vs)
             | otherwise = loop xs ((Single x):((Multiple n v):vs))

--Problem 15 (because 14 is a call to 15 and I'm batman).
myRepeat :: a -> Int -> [a]
myRepeat v n = loop v n []
    where loop v 0 acc = acc
          loop v n acc = loop v (n-1) (v:acc)

repli :: [a] -> Int -> [a]
repli ar n = concat $ map (\v -> myRepeat v n) ar

--Problem 14 *yawn*
dupli ar = repli ar 2

--Problem 16
drop_every :: [a] -> Int -> [a]
drop_every arr v = myReverse $ loop arr v 1 []
    where loop [] v n acc = acc
          loop (x:xs) v n acc
             | n `mod` v == 0 = loop xs v (n+1) acc
             | otherwise = loop xs v (n+1) (x:acc)

--Problem 17
split_at :: [a] -> Int -> ([a], [a])
split_at arr val = let tpl = loop arr val [] in (myReverse $ fst tpl, snd tpl)
    where loop arr 0 acc = (acc, arr)
          loop [] v acc = (acc, [])
          loop (x:xs) v acc = loop xs (v-1) (x:acc)

--Problem 18
slice :: [a] -> Int -> Int -> [a]
slice arr st en = myReverse $ loop arr (st - 1) en []
    where loop [] st en acc = acc
          loop arr 0 1 acc = acc
          loop (x:xs) 0 en acc = loop xs 0 (en - 1) (x:acc)
          loop (x:xs) st en acc = loop xs (st - 1) en acc

--Problem 19
rotate :: [a] -> Int -> [a]
rotate arr 0 = arr
rotate [] n = []
rotate (x:xs) n
     | n > 0 = rotate (xs ++ [x]) (n - 1)
     | otherwise = rotate (x:xs) ((myLength (x:xs)) + n)

--Problem 20 (only non-tail-recursive one so far...)
remove_nth :: [a] -> Int -> (a, [a])
remove_nth [] n = error "nothing to remove"
remove_nth (x:xs) 1 = (x, xs)
remove_nth (x:xs) n = let rec = remove_nth xs (n - 1) in (fst rec, x:(snd rec))
