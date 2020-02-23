
is_palindrome x = x == (reverse x)

encode_in_base b x = (x, reverse $ encode_loop b x)
    where encode_loop base 0 = ""
          encode_loop base x = (show $ x `mod` base) ++ (encode_loop base (x `quot` base))

palindromes_in_base b = filter (is_palindrome . snd) $ map (encode_in_base b) [1..]

seq_of_diffs s = zipWith (-) (map fst $ drop 1 s) $ map fst s

-- chunk_while pred []     = [[]]
-- chunk_while pred (x:xs) = 
