import Data.Bits

nextSubset :: [Bool] -> [Bool]
nextSubset []         = [True]
nextSubset (True:xs)  = False:(nextSubset xs)
nextSubset (False:xs) = True:xs

subsets :: [a] -> [[a]]
subsets xs = map (map fst . filter snd
                          . zip xs)
                 $ iterate nextSubset []

finiteSubsets :: [a] -> [[a]]
finiteSubsets xs = take (2 ^ (length xs)) $ subsets xs


powerset :: [a] -> [[a]]
powerset xs = map (get_subset xs) [(0::Integer)..]
    where get_subset xs x = map snd
                            $ filter (testBit x . fst)
                            $ zip [0..] xs
