
section chapter_2_experiment

/- α is implicit now so this lets it mostly hide. -/
variable {α : Type}

def doOnceMore (f nTimes : α → α) x := f (nTimes x)

def doTwice (f : α → α) := doOnceMore f f

#check @doOnceMore
#check doTwice

end chapter_2_experiment

section chapter_4_exercises

section constructive
variable (p q r : Prop)

-- Double-negation implies LEM (from the text)
example:  ( ¬ ¬ p → p) → (p ∨ ¬ p) := sorry

-- commutativity of ∧ and ∨
example : p ∧ q ↔ q ∧ p :=
  Iff.intro
    (fun hpq => ⟨hpq.right, hpq.left⟩)
    (fun hqp => ⟨hqp.right, hqp.left⟩)

example : (p ∨ q) ↔ (q ∨ p) :=
  Iff.intro
    (fun hl => Or.elim hl
      (fun hp => Or.inr hp)
      (fun hq => Or.inl hq))
    (fun hr => Or.elim hr
      (fun hq => Or.inr hq)
      (fun hq => Or.inl hq))

-- associativity of ∧ and ∨
example : (p ∧ q) ∧ r ↔ p ∧ (q ∧ r) :=
  Iff.intro
    (fun pqtr =>
      have hpq := pqtr.left
      have hr := pqtr.right
      have hp := hpq.left
      have hq := hpq.right
      show p ∧ (q ∧ r) from ⟨hp, ⟨hq, hr⟩⟩)
    (fun ptqr =>
      have hqr := ptqr.right
      have hr := hqr.right
      have hq := hqr.left
      have hp := ptqr.left
      show (p ∧ q) ∧ r from ⟨⟨hp, hq⟩, hr⟩)

example : (p ∨ q) ∨ r ↔ p ∨ (q ∨ r) :=
  Iff.intro
    (fun pqtr => Or.elim pqtr
      (fun hpq => Or.elim hpq
        Or.inl
        (fun hq => Or.inr (Or.inl hq)))
      (fun hr => (Or.inr (Or.inr hr))))
    (fun ptqr => Or.elim ptqr
      (fun hp => (Or.inl (Or.inl hp)))
      (fun hqr => Or.elim hqr
        (fun hq => Or.inl (Or.inr hq))
        Or.inr))

-- distributivity
example : p ∧ (q ∨ r) ↔ (p ∧ q) ∨ (p ∧ r) :=
  Iff.intro
    (fun hfac =>
      have hp := hfac.left
      have hqr := hfac.right
      Or.elim hqr
        (fun hq => Or.inl ⟨hp, hq⟩)
        (fun hr => Or.inr ⟨hp, hr⟩))
    (fun hspread => Or.elim hspread
      (fun hpq =>
        have hp := hpq.left
        have hq := hpq.right
        ⟨hp, Or.inl hq⟩)
      (fun hpr =>
        have hp := hpr.left
        have hr := hpr.right
        ⟨hp, Or.inr hr⟩))

example : p ∨ (q ∧ r) ↔ (p ∨ q) ∧ (p ∨ r) := Iff.intro
  (fun hfac => Or.elim hfac
    (fun hp => ⟨Or.inl hp, Or.inl hp⟩)
    (fun hqr =>
      have hq := hqr.left; have hr := hqr.right
      ⟨Or.inr hq, Or.inr hr⟩))
  (fun hspread =>
    -- Curiosity: does the order of these matter?
    have hpq := hspread.left; have hpr := hspread.right
    Or.elim hpq
      Or.inl
      (fun hq => Or.elim hpr
        Or.inl
        (fun hr => Or.inr ⟨hq, hr⟩)))

-- other properties
example : (p → (q → r)) ↔ (p ∧ q → r) := Iff.intro
  (fun hpqrfun =>
    λ hpandq =>
      have hp := hpandq.left; have hq := hpandq.right
      (hpqrfun hp) hq)
  (fun hpandqfn => λ hp => λ hq => (hpandqfn ⟨hp, hq⟩))

example : ((p ∨ q) → r) ↔ (p → r) ∧ (q → r) := Iff.intro
  (fun hporqfn => ⟨λ hp => hporqfn (Or.inl hp), λ hq => hporqfn (Or.inr hq)⟩)
  (fun hprqrfn =>
    have hprfn := hprqrfn.left; have hqrfn := hprqrfn.right
    λ hporq => Or.elim hporq hprfn hqrfn)

example : ¬(p ∨ q) ↔ ¬p ∧ ¬q := sorry
example : ¬p ∨ ¬q → ¬(p ∧ q) := sorry
example : ¬(p ∧ ¬p) := sorry
example : p ∧ ¬q → ¬(p → q) := sorry
example : ¬p → (p → q) := sorry
example : (¬p ∨ q) → (p → q) := sorry
example : p ∨ False ↔ p := sorry
example : p ∧ False ↔ False := sorry
example : (p → q) → (¬q → ¬p) := sorry

-- From the bottom, after the classical exercises
example : ¬ (p ↔ ¬ p) := sorry

end constructive
section non_constructive

open Classical

variable (p q r : Prop)

example : (p → q ∨ r) → ((p → q) ∨ (p → r)) := sorry
example : ¬(p ∧ q) → ¬p ∨ ¬q := sorry
example : ¬(p → q) → p ∧ ¬q := sorry
example : (p → q) → (¬p ∨ q) := sorry
example : (¬q → ¬p) → (p → q) := sorry
example : p ∨ ¬p := sorry
example : (((p → q) → p) → p) := sorry

end non_constructive
end chapter_4_exercises
