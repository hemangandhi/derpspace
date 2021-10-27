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