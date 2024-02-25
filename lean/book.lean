
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

example : ¬(p ∨ q) ↔ ¬p ∧ ¬q := Iff.intro
  (fun hnpq => ⟨fun hp => hnpq (Or.inl hp), fun hq => hnpq (Or.inr hq)⟩)
  (fun hnpnq =>
    have hnp := hnpnq.left; have hnq := hnpnq.right
    (fun hpq => Or.elim hpq hnp hnq))

example : ¬p ∨ ¬q → ¬(p ∧ q) :=
  λ hnprnq => Or.elim hnprnq
    (fun hnp => λ hpandq => hnp hpandq.left)
    (fun hnq => λ hpandq => hnq hpandq.right)

example : ¬(p ∧ ¬p) := λ hpanp => hpanp.right hpanp.left

example : p ∧ ¬q → ¬(p → q) := λ hpanq => λ hpiq => hpanq.right (hpiq hpanq.left)

example : ¬p → (p → q) := λ hnp => λ hp => False.elim (hnp hp)

example : (¬p ∨ q) → (p → q) := λ hnprq => Or.elim hnprq
  (fun hnp => λ hp => False.elim (hnp hp))
  (fun hq => λ _hp => hq)

example : p ∨ False ↔ p := Iff.intro
  (fun hprf => Or.elim hprf (fun hp => hp) False.elim)
  Or.inl

example : p ∧ False ↔ False := Iff.intro
  (fun hpaf => False.elim hpaf.right)
  (fun hf => ⟨False.elim hf, hf⟩)

example : (p → q) → (¬q → ¬p) := λ hpiq => λ hnq => λ hp => hnq (hpiq hp)

-- From the bottom, after the classical exercises
example : ¬ (p ↔ ¬ p) :=
  λ hpenp =>
    have hpinp := hpenp.mp; have hnpip := hpenp.mpr
    have hnp : ¬p := (λ hp => (hpinp hp) hp)
    absurd (hnpip hnp) hnp

-- Double-negation implies LEM (from the text)
example: ({x : Prop} → ¬¬x → x) → (p ∨ ¬p) := λ dne =>
  suffices hdd : ¬¬(p ∨ ¬p) from dne hdd
  (λ hnlem => hnlem (Or.inr (λ hp => absurd (Or.inl hp) hnlem)))


end constructive
section non_constructive

open Classical

variable (p q r : Prop)

example : (p → q ∨ r) → ((p → q) ∨ (p → r)) := λ hpiqr =>
  byCases
    (fun h : q => Or.inl (λ _h : p => h))
    (fun h : ¬q => Or.inr (λ hq : p => Or.elim (hpiqr hq)
      (fun hq: q => False.elim (h hq))
      (fun hr: r => hr)))

example : ¬(p ∧ q) → ¬p ∨ ¬q := λ hnpaq =>
  byCases
    (fun hp : p => byCases
      (fun hq : q => show ¬p ∨ ¬q from False.elim (hnpaq ⟨hp, hq⟩))
      (fun hnq : ¬q => Or.inr hnq))
    (fun hnp : ¬p => Or.inl hnp)

example : ¬(p → q) → p ∧ ¬q := λ hnpiq =>
  byCases
    (fun hq : q => absurd (λ _h => hq) hnpiq)
    (fun hnq : ¬q => byCases
      (fun hp : p => ⟨hp, hnq⟩)
      (fun hnp : ¬p => absurd (λ hp₂ => absurd hp₂ hnp) hnpiq))

example : (p → q) → (¬p ∨ q) := λ hpiq =>
  byCases
    (fun hp : p => Or.inr (hpiq hp))
    (fun hnp : ¬p => Or.inl hnp)

example : (¬q → ¬p) → (p → q) := fun hnqinp =>
  λ hp : p => byCases
    (fun hq : q => hq)
    (fun hnq : ¬q => absurd hp (hnqinp hnq))

-- Circular?
example : p ∨ ¬p := byCases Or.inl Or.inr

example : (((p → q) → p) → p) := λ hpqp =>
  byCases
    (fun hp : p => hp)
    (fun hnp : ¬p => absurd (hpqp (λ hp₂ => absurd hp₂ hnp)) hnp)

end non_constructive
end chapter_4_exercises
