
section chapter_2_experiment

/- α is implicit now so this lets it mostly hide. -/
variable {α : Type}

def doOnceMore (f nTimes : α → α) x := f (nTimes x)

def doTwice (f : α → α) := doOnceMore f f

#check @doOnceMore
#check doTwice

end chapter_2_experiment

section chapter_3_exercises

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
end chapter_3_exercises

section chpater_4_exercises
section exercise_1

variable (α : Type) (p q : α → Prop)

-- TODO: consider just calling hx x?
example : (∀ x, p x ∧ q x) ↔ (∀ x, p x) ∧ (∀ x, q x) := Iff.intro
  (fun hxpq => ⟨λ hx => (hxpq hx).left, λ hx => (hxpq hx).right⟩)
  (fun ⟨hxp, hxq⟩ => λ hx => ⟨hxp hx, hxq hx⟩)

example : (∀ x, p x → q x) → (∀ x, p x) → (∀ x, q x) :=
  fun hxpq => fun hxp => λ x => hxpq x (hxp x)

example : (∀ x, p x) ∨ (∀ x, q x) → ∀ x, p x ∨ q x := fun hxpxq =>
  Or.elim hxpxq
    (fun hxp => λ x => Or.inl (hxp x))
    (fun hxq => λ x => Or.inr (hxq x))

end exercise_1

section exercise_2

variable (α : Type) (p q : α → Prop)
variable (r : Prop)

example : α → ((∀ _x : α, r) ↔ r) := fun x =>
  Iff.intro
    (fun fx => fx x)
    (fun r => λ _xx => r)

open Classical

example : (∀ x, p x ∨ r) ↔ (∀ x, p x) ∨ r := Iff.intro
  (fun fx => byCases Or.inr
    (fun hnr => Or.inl (
      λ x => Or.elim (fx x)
        (fun px => px)
        (fun hr => absurd hr hnr))))
  (fun fxhr => Or.elim fxhr
    (fun fx => λ x => Or.inl (fx x))
    (fun hr => λ _x => Or.inr hr))

example : (∀ x, r → p x) ↔ (r → ∀ x, p x) := Iff.intro
  (fun hxhrp => λ hr => λ hx => hxhrp hx hr)
  (fun hrxp => λ hx => λ hr => hrxp hr hx)

end exercise_2
section exercise_3

variable (men : Type) (barber : men)
variable (shaves : men → men → Prop)

example (h : ∀ x : men, shaves barber x ↔ ¬ shaves x x) : False :=
  have who_shaves_barber := h barber
  have not_barber := who_shaves_barber.mp
  have not_himself := who_shaves_barber.mpr
  have if_barber_himself := λ bb => (not_barber bb) bb
  if_barber_himself (not_himself if_barber_himself)

end exercise_3
section exercise_4

def even (n : Nat) : Prop := ∃ k : Nat, 2 * k = n

def prime (n : Nat) : Prop :=
  n > 1 ∧ (∀ p : Nat, 1 < p ∧ p < n → (¬ ∃ m, m * p = n))

def infinitely_many_primes : Prop := ∀ n : Nat, ∃ p : Nat, prime p ∧ p > n

def Fermat_prime (n : Nat) : Prop := prime n ∧ ∃ k, 2 ^ (2^k) + 1 = n

def infinitely_many_Fermat_primes : Prop :=
  ∀ n : Nat, ∃ p : Nat, Fermat_prime p ∧ p > n

def Goldbach_conjecture : Prop :=
  ∀ n : Nat, n ≤ 2 ∨ (∃ p q, prime p ∧ prime q ∧ p + q = n)

def Goldbach's_weak_conjecture : Prop :=
  let odd_prime := (fun (n : Nat) => (¬ even n) ∧ (prime n));
  ∀ n : Nat, n ≤ 7 ∨ (∃ p q r, odd_prime p ∧ odd_prime q ∧ odd_prime r ∧ p + q + r = n)

def Fermat's_last_theorem : Prop :=
  ∀ n a b c : Nat, (a^n + b^n = c^n → n ≤ 2)

end exercise_4
section exercise_5

open Classical

variable (α : Type) (p q : α → Prop)
variable (r : Prop)

example : (∃ _x : α, r) → r := λ ⟨_ex, hr⟩ => hr

example (a : α) : r → (∃ _x : α, r) := λ hr => ⟨a, hr⟩

example : (∃ x, p x ∧ r) ↔ (∃ x, p x) ∧ r := Iff.intro
  (fun ⟨x, px, r⟩ => ⟨⟨x, px⟩, r⟩)
  (fun ⟨⟨x, px⟩, r⟩ => ⟨x, px, r⟩)

example : (∃ x, p x ∨ q x) ↔ (∃ x, p x) ∨ (∃ x, q x) := Iff.intro
  (fun ⟨x, pq⟩ => Or.elim pq
    (fun hp => Or.inl ⟨x, hp⟩)
    (fun hq => Or.inr ⟨x, hq⟩))
  (fun xpq => Or.elim xpq
    (fun ⟨x, hp⟩ => ⟨x, Or.inl hp⟩)
    (fun ⟨x, hq⟩ => ⟨x, Or.inr hq⟩))


example : (∀ x, p x) ↔ ¬ (∃ x, ¬ p x) := Iff.intro
  (fun all => λ ⟨x, npx⟩ => absurd (all x) npx)
  (fun nex => λ x => byCases
    (fun px => px)
    (fun npx => absurd ⟨x, npx⟩ nex))

example : (∃ x, p x) ↔ ¬ (∀ x, ¬ p x) := Iff.intro
  (fun ⟨x, px⟩ => λ all => absurd px (all x))
  (fun nall => byContradiction
    (fun nex =>
      suffices all : (∀ x, ¬ p x) from (absurd all nall)
      λ x => byCases
        (fun px => absurd ⟨x, px⟩ nex)
        (fun npx => npx)))

example : (¬ ∃ x, p x) ↔ (∀ x, ¬ p x) := Iff.intro
  (fun nex => λ x => λ px => nex ⟨x, px⟩)
  (fun all => λ ⟨x, px⟩ => (all x) px)

example : (¬ ∀ x, p x) ↔ (∃ x, ¬ p x) := Iff.intro
  (fun nall => byContradiction
    (fun nex => suffices all : (∀ x, p x) from (absurd all nall)
      λ x => byCases (fun px => px) (fun npx => absurd ⟨x, npx⟩ nex)))
  (fun ⟨x, npx⟩ => λ all => npx (all x))


example : (∀ x, p x → r) ↔ (∃ x, p x) → r := Iff.intro
  (fun all => λ ⟨x, px⟩ => all x px)
  (fun epr => λ x => λ px => epr ⟨x, px⟩)

example (a : α) : (∃ x, p x → r) ↔ (∀ x, p x) → r := Iff.intro
  (fun ⟨x, pr⟩ => λ all => pr (all x))
  -- Gave up and copied because... WTF
  (fun h1 : (∀ x, p x) → r => show ∃ x, p x → r from
    byCases
      (fun hap : ∀ x, p x => ⟨a, λ _h' => h1 hap⟩)
      -- Really we just use a prior exercise here.
      (fun hnap : ¬ ∀ x, p x =>
        byContradiction
          (fun hnex : ¬ ∃ x, p x → r =>
            have hap : ∀ x, p x := fun x => byContradiction
              (fun hnp : ¬ p x => have hex : ∃ x, p x → r := ⟨x, (fun hp => absurd hp hnp)⟩
                 show False from hnex hex)
              show False from hnap hap)))

example (a : α) : (∃ x, r → p x) ↔ (r → ∃ x, p x) := Iff.intro
  (fun ⟨x, rp⟩ => λ hr => ⟨x, rp hr⟩)
  (fun rep => show (∃ x, r → p x) from byCases
    (fun hr : r => have ⟨x, px⟩ := rep hr; ⟨x, λ _hr => px⟩)
    (fun hnr : ¬r => byContradiction
      (fun hnex : ¬(∃ x, r → p x) =>
        have alln : (∀ x, ¬(r → p x)) := λ x => λ px => hnex ⟨x, px⟩
        have nrpa : ¬(r → p a) := alln a
        suffices hrp : (r ∧ ¬ (p a)) from hnr hrp.left
        ⟨byContradiction (fun hnr => nrpa (λ hr => absurd hr hnr)),
         byContradiction (fun nnpa => nrpa (λ _hr => byCases
           (fun pa => pa) (fun npa => absurd npa nnpa)))⟩)))

end exercise_5
end chpater_4_exercises