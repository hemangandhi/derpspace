
cond :: [(Bool, t)] -> t
cond vals = if null vals
            then error 
