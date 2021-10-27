import Data.Maybe

type SubsetIndex = [Maybe Bool]

nextSubset :: SubsetIndex -> SubsetIndex
nextSubset [] = repeat Nothing
nextSubset ((Just True):xs)  = (Just False):(nextSubset xs)
nextSubset (Nothing:xs)      = (Just True):xs
nextSubset ((Just False):xs) = (Just True):xs

subsets :: [a] -> [[a]]
subsets xs = map (map fst . filter (fromMaybe False . snd)
                          . takeWhile (isJust . snd)
			  . zip xs)
                 $ iterate nextSubset $ nextSubset []

finite_subsets :: [a] -> [[a]]
finite_subsets xs = take (2 ^ (length xs)) $ subsets xs