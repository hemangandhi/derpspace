import qualified Data.Map
import Prelude


factorial :: (Num a, Eq a) => a -> a
factorial 0 = 1
factorial n = n * (factorial $ n - 1)


tabulate :: (Ord a) => [a] -> Data.Map.Map a Int
tabulate = foldr updateMap Data.Map.empty
    where updateMap a = Data.Map.insertWith ((+)) a 1


-- A sorted list of integers (from ascending to descending).
-- The idea is "frequencies of characters that can be used to make up permutations."
-- NOTE: I expect that if we never care about which character appears when, we'll over-count permuting duplicate characters consistently.
-- Also, it gives Tableau (like Young Tableau) but I'm not sure if it has anything to do with this.
-- Frequency of character, frequency of that frequency.
type Tableau = [(Int, Int)]


tableauOfString :: [Char] -> Tableau
tableauOfString = Data.Map.toList . tabulate . Data.Map.elems . tabulate


kSubsetsOfTableau :: Int -> Tableau -> [Tableau]
kSubsetsOfTableau 0 t             = [[]]
kSubsetsOfTableau k []            = []
kSubsetsOfTableau k ((t, f):ts)  = foldr (++) [] $ map (recur ts k t)
                                                 $ takeWhile ((>=) (min k f))
                                                 $ iterate ((+) 1) 0
    where recur ts k t 0 = kSubsetsOfTableau k ts
          recur ts k t f = map ((:) (t, f)) $ kSubsetsOfTableau (k - f) ts


palindromesOfSubset :: Tableau -> Int
palindromesOfSubset [] = 0
palindromesOfSubset ts = foldr (*) 1 $ map powChoose2 ts
    where powChoose2 (t, f) = (t * (t - 1) `div` 2) ^ f


-- TODO: this is wrong. Probably because the counting thing only works out in terms of prefixes?
-- Not really sure.
--
-- ghci> nAvoidKPalindrome 3 2 "abb"
-- 6
-- ghci> tableauOfString "abb"
-- [(1,1),(2,1)]
-- ghci> kSubsetsOfTableau 2 $ tableauOfString "abb"
-- [[(1,1),(2,1)]]
--
--
nAvoidKPalindrome :: Int -> Int -> [Char] -> Int
nAvoidKPalindrome n k = (-) (factorial n) . sum
                                          . map palindromesOfSubset
                                          . kSubsetsOfTableau k
                                          . tableauOfString

main :: IO ()
main = do
    [n, k] <- (map read . words) <$> getLine
    s <- getLine
    putStrLn $ show $ nAvoidKPalindrome n k s

