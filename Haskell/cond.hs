
cond :: [(Bool, t)] -> t
cond [] = error "no matching case"
cond ((True,v):mr) = v
cond ((False,v):mr) = cond mr
