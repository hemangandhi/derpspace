-- From http://book.realworldhaskell.org/read/types-and-functions.html

myDrop :: Int -> [a] -> [a]
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) xs

myTake :: Int -> [a] -> [a]
myTake n xs = if n <= 0 || null xs
              then []
              else (head xs) : (myTake (n - 1) (tail xs))

-- Exercise 3

lastButOne :: [a] -> a
lastButOne xs = if null (tail (tail xs))
                then head xs
                else lastButOne (tail xs)
