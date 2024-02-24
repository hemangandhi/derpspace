/- Test? -/

/- α is implicit now so this lets it mostly hide. -/
variable {α : Type}

def doOnceMore (f nTimes : α → α) x := f (nTimes x)

def doTwice (f : α → α) := doOnceMore f f

#check @doOnceMore
#check doTwice
