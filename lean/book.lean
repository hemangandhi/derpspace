/- Test? -/

variable (α : Type)

def doOnceMore (nTimes f : α → α) : (α → α) :=
  λ x => f (nTimes x)

/- Lesson: you need a α here explicitly. Weird. -/
def doTwice (f : α → α) : (α → α) := doOnceMore α f f

