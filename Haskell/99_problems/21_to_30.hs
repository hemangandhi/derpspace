import System.Random

--Problem 3 (for 23)
elementAt :: [a] -> Int -> a
elementAt _ 0 = error "Indexing from 1"
elementAt [] _ = error "Out of bounds"
elementAt (x:xs) 1 = x
elementAt (x:xs) i = elementAt xs (i - 1)

--Problem 4 (for 23)
myLength :: [a] -> Int
myLength [] = 0
myLength x = tailRec x 0
    where tailRec [] v = v
          tailRec (x:xs) v = tailRec xs (v + 1)

--Problem 5 (for better tail recursive functions)
myReverse :: [a] -> [a]
myReverse arr = tailRec arr []
    where tailRec [] a = a
          tailRec (x:xs) acc = tailRec xs (x:acc)

--Problem 21 (also not tail recursive)
insertAt :: a -> [a] -> Int -> [a]
insertAt v [] n = [v]
insertAt v arr 1 = (v:arr)
insertAt v (x:xs) n = (x:(insertAt v xs $ n - 1))

--Tail recursive version
insertAtB :: a -> [a] -> Int -> [a]
insertAtB v arr n = loop v n arr []
    where loop v n [] acc = (myReverse acc) ++ [v]
          loop v 1 arr acc = (myReverse acc) ++ [v] ++ arr
          loop v n (x:xs) acc = loop v (n - 1) xs (x:acc)

--Problem 22
range :: Int -> Int -> [Int]
range x y = myReverse $ loop x y []
    where loop x y arr
             | x == y = (y:arr)
             | otherwise = loop (x + 1) y (x:arr)

--Better version?
betterRange :: Int -> Int -> [Int]
betterRange x y = [x .. y]

--Problem 23
rndSelect :: [a] -> Int -> [a]
rndSelect [] t = []
rndSelect arr t = map (elementAt arr) (take t $ (randomRs (1, myLength arr) (mkStdGen t)))

--Problem 20 (only non-tail-recursive one so far...)
remove_nth :: [a] -> Int -> (a, [a])
remove_nth [] n = error "nothing to remove"
remove_nth (x:xs) 1 = (x, xs)
remove_nth (x:xs) n = let rec = remove_nth xs (n - 1) in (fst rec, x:(snd rec))

--Problem 24
rndFromRange :: Int -> Int -> [Int]
rndFromRange n m 
           | n > m = error "Repeats would exist!"
           | otherwise = foldl (fst . remove_nth) [1 .. m] $ take (m - n) $ (randomRs (1, myLength arr) (mkStdGen $ n*M))
