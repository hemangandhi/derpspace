-- Problem 21

isAmicable :: Int -> (Int, Bool)
isAmicable x = (z, x == z)
             where y = sum [v | v <- [1..x], x `mod` v == 0]
                   z = sum [v | v <- [1..y], y `mod` v == 0]

amSums x = sum [v | v <- [1..x], (snd $ isAmicable v) ==  True]
