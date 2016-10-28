
--Problem 1
myLast :: [a] -> a
myLast [] = error "no last of empty list"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

--Problem 2
myButLast :: [a] -> a
myButLast [] = error "no but last of empty list"
myButLast (x:(y:[])) = x
myButLast (x:xs) = myButLast xs

--Problem 3
elementAt :: [a] -> Int -> a
elementAt _ 0 = error "Indexing from 1"
elementAt [] _ = error "Out of bounds"
elementAt (x:xs) 1 = x
elementAt (x:xs) i = elementAt xs (i - 1)

--Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength x = tailRec x 0
    where tailRec [] v = v
          tailRec (x:xs) v = tailRec xs (v + 1)

--Problem 5
myReverse :: [a] -> [a]
myReverse arr = tailRec arr []
    where tailRec [] a = a
          tailRec (x:xs) acc = tailRec xs (x:acc)

--Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome arr = (arr == (myReverse arr))

--Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List a) = concat $ map flatten a

--Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x:xs) = myReverse $ iter xs [x]
    where iter [] acc = acc
          iter (x:xs) (a:as)
             | x == a    = iter xs (a:as)
             | otherwise = iter xs (x:(a:as))

--Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = [[]]
pack (x:[]) = [[x]]
pack (x:xs) = tail $ myReverse $ loop xs [x] [[]]
    where loop [] t acc = t:acc
          loop (x:xs) (t:ts) acc
             | x == t = loop xs (x:(t:ts)) acc
             | otherwise = loop xs [x] ((t:ts):acc)
