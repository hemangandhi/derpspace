
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

section chapter_5_exercises
section chapter_3_redo

variable (p q r : Prop)

example : (p ∨ q) ↔ (q ∨ p) := by
  apply Iff.intro
  . intro
    | Or.inl hp => exact Or.inr hp
    | Or.inr hq => exact Or.inl hq
  . intro
    | Or.inl hq => exact Or.inr hq
    | Or.inr hp => exact Or.inl hp

example : (p ∧ q) ∧ r ↔ p ∧ (q ∧ r) := by
  apply Iff.intro
  . intro ⟨⟨hp, hq⟩, hr⟩
    exact ⟨hp, ⟨hq, hr⟩⟩
  . intro ⟨hp, ⟨hq, hr⟩⟩
    exact ⟨⟨hp, hq⟩, hr⟩

example : (p ∨ q) ∨ r ↔ p ∨ (q ∨ r) := by
  apply Iff.intro
  . intro
    | Or.inl (Or.inl hp) => exact Or.inl hp
    | Or.inl (Or.inr hq) => apply Or.inr; exact (Or.inl hq)
    | Or.inr hr => apply Or.inr; exact (Or.inr hr)
  . intro
    | Or.inl hp => apply Or.inl; exact Or.inl hp
    | Or.inr (Or.inl hq) => apply Or.inl; exact (Or.inr hq)
    | Or.inr (Or.inr hr) => exact Or.inr hr

example : p ∧ (q ∨ r) ↔ (p ∧ q) ∨ (p ∧ r) := by
  apply Iff.intro
  . intro
    | ⟨hp, Or.inl hq⟩ => exact Or.inl ⟨hp, hq⟩
    | ⟨hp, Or.inr hr⟩ => exact Or.inr ⟨hp, hr⟩
  . intro
    | Or.inl ⟨hp, hq⟩ => exact ⟨hp, Or.inl hq⟩
    | Or.inr ⟨hp, hr⟩ => exact ⟨hp, Or.inr hr⟩

example : p ∨ (q ∧ r) ↔ (p ∨ q) ∧ (p ∨ r) := by
  apply Iff.intro
  . intro
    | Or.inl hp => apply And.intro <;> exact Or.inl hp
    | Or.inr ⟨hq, hr⟩ => apply And.intro; exact Or.inr hq; exact Or.inr hr
  . intro
    | ⟨Or.inr hq, Or.inr hr⟩ => exact Or.inr ⟨hq, hr⟩
    -- TODO: is there a way to write this so that the two below can be like <;>
    | ⟨Or.inl hp, _⟩ => apply Or.inl; assumption
    | ⟨_, Or.inl hp⟩ => apply Or.inl; assumption


end chapter_3_redo
section chapter_4_redo

variable (α : Type) (p q : α → Prop)
variable (r : Prop)

open Classical

example : (∀ x, p x ∨ r) ↔ (∀ x, p x) ∨ r := by
  apply Iff.intro
  . intro fx
    apply byCases
    case hpq => exact Or.inr
    case hnpq =>
      intro hnr
      apply Or.inl
      . intro x
        cases (fx x) with
        | inl hpx => exact hpx
        | inr hr => exact absurd hr hnr
  . intro
    | Or.inl fxp => intro x; exact Or.inl (fxp x)
    | Or.inr r => intros; apply Or.inr; exact r

example : (∃ x, p x ∨ q x) ↔ (∃ x, p x) ∨ (∃ x, q x) := by
  apply Iff.intro
  . intro
    | ⟨x, Or.inl hp⟩ => exact Or.inl ⟨x, hp⟩
    | ⟨x, Or.inr hq⟩ => exact Or.inr ⟨x, hq⟩
  . intro
    | Or.inl ⟨x, hp⟩ => exact ⟨x, Or.inl hp⟩
    | Or.inr ⟨x, hq⟩ => exact ⟨x, Or.inr hq⟩


example : (∀ x, p x) ↔ ¬ (∃ x, ¬ p x) := by
  apply Iff.intro
  . intro all; exact λ ⟨x, npx⟩ => absurd (all x) npx
  . intro nex; exact (λ x => by
      apply byCases
      case hpq => exact λ px => px
      case hnpq => exact λ npx => absurd ⟨x, npx⟩ nex)

example : (∃ x, p x) ↔ ¬ (∀ x, ¬ p x) := by
  apply Iff.intro
  . intro ⟨x, px⟩; exact λ all => absurd px (all x)
  . intro nall; apply byContradiction
    . intro nex
      suffices all : (∀ x, ¬ p x) from (absurd all nall)
      intro x
      apply byCases
      case hpq => exact (fun px => absurd ⟨x, px⟩ nex)
      case hnpq => exact (fun npx => npx)

example : (¬ ∀ x, p x) ↔ (∃ x, ¬ p x) := by
  apply Iff.intro
  case mpr => intro ⟨x, npx⟩; exact λ all => npx (all x)
  . intro nall
    apply byContradiction
    intro nex
    suffices all : (∀ x, p x) from (absurd all nall)
    exact (λ x => by
      apply byCases
      case hpq => intro px; assumption
      case hnpq => intro npx; exact absurd ⟨x, npx⟩ nex)

example (a : α) : (∃ x, p x → r) ↔ (∀ x, p x) → r := by
  apply Iff.intro
  . intro ⟨x, pr⟩; exact λ all => pr (all x)
  . intro h1
    show ∃ x, p x → r
    apply byCases
    case hpq => intro hap; exact ⟨a, λ _h' => h1 hap⟩
    case hnpq =>
      intro hnap
      apply byContradiction
      intro hnex
      suffices hap : ∀ x, p x from hnap hap
      intro x
      apply byContradiction
      intro hnp
      have hex : ∃ x, p x → r := ⟨x, (fun hp => absurd hp hnp)⟩
      exact hnex hex

example (a : α) : (∃ x, r → p x) ↔ (r → ∃ x, p x) := by
  apply Iff.intro
  . intro ⟨x, rp⟩; exact λ hr => ⟨x, rp hr⟩
  . intro rep
    show (∃ x, r → p x)
    apply byCases
    . intro hr
      have ⟨x, px⟩ := rep hr;
      exact ⟨x, λ _hr => px⟩
    . intro hnr
      apply byContradiction
      intro hnex
      have alln : (∀ x, ¬(r → p x)) := λ x => λ px => hnex ⟨x, px⟩
      have nrpa : ¬(r → p a) := alln a
      suffices hrp : (r ∧ ¬ (p a)) from hnr hrp.left
      apply And.intro
      . apply byContradiction
        intro hnr
        exact nrpa (λ hr => absurd hr hnr)
      . apply byContradiction
        intro nnpa
        apply nrpa
        intros
        apply byCases
        . intro pa; assumption
        . intro npa; exact absurd npa nnpa

end chapter_4_redo


example (p q r : Prop) (hp : p)
        : (p ∨ q ∨ r) ∧ (q ∨ p ∨ r) ∧ (q ∨ r ∨ p) := by
  exact ⟨Or.inl hp, Or.inr (Or.inl hp), Or.inr (Or.inr hp)⟩

end chapter_5_exercises

section chapter_7_exercises
-- Hide the Nat stuff.
namespace exercise_1

variable (x y z : Nat)

def mult (x y : Nat) : Nat :=
  match x, y with
  | Nat.zero,     _ => Nat.zero
  | (Nat.succ x), y => y + (mult x y)

theorem zero_mult : mult 0 y = 0 := by rfl

theorem mult_zero : mult x 0 = 0 :=
  Nat.recOn (motive := λ n => mult n 0 = 0) x
    (show mult 0 0 = 0 by rfl)
    (fun (x : Nat) (ih : mult x 0 = 0) =>
      show mult (Nat.succ x) 0 = 0 by
      calc mult (Nat.succ x) 0
        _ = 0 + (mult x 0) := rfl
        _ = 0 + 0          := by rw [ih]
        _ = 0              := by rw [Nat.add_zero])

theorem mult_succ : mult x (Nat.succ y) = (mult x y) + x :=
  Nat.recOn (motive := λ n => mult n (Nat.succ y) = (mult n y) + n) x
    (show mult 0 (Nat.succ y) = (mult 0 y) + 0 by rfl)
    (fun (x : Nat) (ih : mult x (Nat.succ y) = (mult x y) + x) =>
      show mult (Nat.succ x) (Nat.succ y) = (mult (Nat.succ x) y) + (Nat.succ x) by
      calc mult (Nat.succ x) (Nat.succ y)
        _ = (Nat.succ y) + mult x (Nat.succ y)   := rfl
        _ = (Nat.succ y) + (mult x y) + x        := by rw [ih, Nat.add_assoc]
        _ = y + 1 + (mult x y) +  x              := by rw [Nat.succ_eq_add_one]
        _ = y + ((mult x y) + (x + 1))           := by rw [Nat.add_assoc, Nat.add_right_comm,
                                                           Nat.add_assoc, Nat.add_assoc]
        _ = y + (mult x y) + (Nat.succ x)        := by rw [←Nat.succ_eq_add_one, ←Nat.add_assoc]
        _ = (mult (Nat.succ x) y) + (Nat.succ x) := rfl)

theorem mult_comm (x y : Nat) : mult x y = mult y x :=
  Nat.recOn (motive := λ n => mult x n = mult n x) y
    (show mult x 0 = mult 0 x by rw [mult_zero, zero_mult])
    (fun (y: Nat) (ih : mult x y = mult y x) =>
      show mult x (Nat.succ y) = mult (Nat.succ y) x by
      calc mult x (Nat.succ y)
        _ = x + mult x y        := by rw [Nat.add_comm, ←mult_succ]
        _ = x + mult y x        := by rw [ih]
        _ = mult (Nat.succ y) x := by rfl)

theorem mult_right_distr : (mult x z + mult y z) = mult (x + y) z :=
  Nat.recOn (motive := λ n => (mult x n + mult y n) = mult (x + y) n) z
    (show (mult x 0 + mult y 0) = mult (x + y) 0 by
     calc mult x 0 + mult y 0
      _ = 0 + 0 := by rw [mult_zero, mult_zero]
      _ = 0 := by rw [Nat.zero_add]
      _ = mult (x + y) 0 := by rw [mult_zero])
    (fun (z: Nat) (ih: (mult x z + mult y z) = mult (x + y) z) =>
      show mult x (Nat.succ z) + mult y (Nat.succ z) = mult (x + y) (Nat.succ z) by
      calc mult x (Nat.succ z) + mult y (Nat.succ z)
        _ = (mult x z) + x + ((mult y z) + y) := by rw [mult_succ, mult_succ]
        _ = (mult x z) + (mult y z) + y + x   := by rw [Nat.add_right_comm, ←Nat.add_assoc]
        _ = (mult (x + y) z) + y + x          := by rw [ih]
        _ = mult (x + y) (Nat.succ z)         := by rw [Nat.add_right_comm, Nat.add_assoc,
                                                        mult_succ])


end exercise_1
section exercise_2

variable (α : Type)
variable (as bs : List α)

theorem part_a : List.length (as ++ bs) = List.length as + List.length bs :=
  List.recOn (motive := λ xs => List.length (xs ++ bs) = List.length xs + List.length bs ) as
    (show List.length (List.nil ++ bs) = List.length List.nil + List.length bs by
    calc List.length (List.nil ++ bs)
      _ = List.length bs := by rw [List.nil_append]
      _ = 0 + List.length bs := by rw [Nat.zero_add]
      _ = List.length List.nil + List.length bs := rfl)
    (fun (a : α) (as : List α) (ih : List.length (as ++ bs) = List.length as + List.length bs) =>
      show List.length ((List.cons a as) ++ bs) = List.length (List.cons a as) + List.length bs by
      calc List.length ((List.cons a as) ++ bs)
        _ = List.length (List.cons a (as ++ bs))          := by rw [List.cons_append]
        _ = Nat.succ (List.length (as ++ bs))             := by rw [List.length_cons]
        _ = Nat.succ (List.length as + List.length bs)    := by rw [ih]
        _ = List.length as + 1 + List.length bs           := by rw [Nat.succ_eq_add_one,
                                                                    Nat.add_comm,
                                                                    Nat.add_left_comm,
                                                                    ←Nat.add_assoc]
        _ = Nat.succ (List.length as) + (List.length bs)  := by rw [←Nat.succ_eq_add_one]
        _ = List.length (List.cons a as) + List.length bs := by rw [←List.length_cons])

theorem part_b : List.length (List.reverse as) = List.length as :=
  List.recOn (motive := λ xs => List.length (List.reverse xs) = List.length xs) as
    (show List.length (List.reverse List.nil) = List.length List.nil by rfl)
    (fun (a : α) (as : List α) (ih : List.length (List.reverse as) = List.length as) =>
      show List.length (List.reverse (List.cons a as)) = List.length (List.cons a as) by
      calc List.length (List.reverse (List.cons a as))
        _ = List.length ((List.reverse as) ++ (List.cons a List.nil))          := by rw [List.reverse_cons]
        _ = List.length (List.reverse as) + List.length (List.cons a List.nil) := by rw [part_a]
        _ = List.length as + List.length (List.cons a List.nil)                := by rw [ih]
        _ = List.length as + 1                                                 := rfl
        _ = List.length (List.cons a as)                                       := by rw [←Nat.succ_eq_add_one,
                                                                                         ←List.length_cons])

theorem part_c: List.reverse (List.reverse as) = as :=
  List.recOn (motive := λ xs => List.reverse (List.reverse xs) = xs) as
    (show List.reverse (List.reverse List.nil) = List.nil by rfl)
    (fun (a : α) (as : List α) (ih : List.reverse (List.reverse as) = as) =>
      show List.reverse (List.reverse (List.cons a as)) = List.cons a as by
      calc List.reverse (List.reverse (List.cons a as))
        _ = List.reverse (List.reverse as ++ (List.cons a List.nil))              := by rw [List.reverse_cons]
        _ = List.reverse (List.cons a List.nil) ++ List.reverse (List.reverse as) := by rw [List.reverse_append]
        _ = List.reverse (List.nil) ++ (List.cons a List.nil) ++ as               := by rw [ih,
                                                                                            List.reverse_cons]
        _ = List.cons a as                                                        := rfl)

end exercise_2
section exercise_3

inductive Expr where
  | const  : (n : Nat) → Expr
  | var    : (id : Nat) → Expr
  | plus   : Expr → Expr → Expr
  | times  : Expr → Expr → Expr
  deriving Repr

def convertedNthOrElse {α β : Type} (xs : List α) (n : Nat) (conv : α → β) (default : β) : β :=
  match xs, n with
  | List.nil,       _           => default
  | List.cons x _xs, Nat.zero   => conv x
  | List.cons _x xs, Nat.succ n => convertedNthOrElse xs n conv default

def applyIfBothConstElse (x y : Expr) (ap : Nat → Nat → Nat) (el : Expr → Expr → Expr) : Expr :=
  match x, y with
  | Expr.const x, Expr.const y => Expr.const (ap x y)
  | l,            r            => el l r

def evalExpr (e : Expr) (binds : List Nat) : Expr :=
  match e with
  | Expr.const x   => Expr.const x
  | Expr.var x     => convertedNthOrElse binds x Expr.const (Expr.var x)
  | Expr.plus x y  => applyIfBothConstElse (evalExpr x binds) (evalExpr y binds)
                                           (λ x y => x + y) Expr.plus
  | Expr.times x y => applyIfBothConstElse (evalExpr x binds) (evalExpr y binds)
                                           (λ x y => x * y) Expr.times

#eval evalExpr (Expr.const 4) List.nil
#eval evalExpr (Expr.var 0) (List.cons 5 List.nil)
#eval evalExpr (Expr.plus (Expr.var 0) (Expr.const 4)) (List.cons 5 List.nil)
#eval evalExpr (Expr.plus (Expr.var 1) (Expr.const 4)) (List.cons 5 List.nil)
#eval evalExpr (Expr.times (Expr.plus (Expr.var 0) (Expr.const 4)) (Expr.var 1)) (List.cons 5 List.nil)
#eval evalExpr (Expr.times (Expr.plus (Expr.var 0) (Expr.const 4)) (Expr.var 1)) (List.cons 5 (List.cons 4 List.nil))

end exercise_3
section exercise_4

inductive PropForm where
  | const : Bool → PropForm
  | var   : Nat → PropForm
  | not   : PropForm → PropForm
  | and   : PropForm → PropForm → PropForm
  | or    : PropForm → PropForm → PropForm
  | imply : PropForm → PropForm → PropForm
  | equiv : PropForm → PropForm → PropForm
  deriving Repr

def evalProp : PropForm → PropForm
  | PropForm.const c => (PropForm.const c)
  | PropForm.var n   => (PropForm.var n)
  | PropForm.not e   => match evalProp e with
    | PropForm.const c => PropForm.const (not c)
    | e                => PropForm.not e
  | PropForm.and p q => match evalProp p, evalProp q with
    | PropForm.const false, _ => PropForm.const false
    | PropForm.const true,  x => x
    | p,                    q => PropForm.and p q
  | PropForm.or  p q => match evalProp p, evalProp q with
    | PropForm.const true,  _ => PropForm.const true
    | PropForm.const false, x => x
    | p,                    q => PropForm.or p q
  | PropForm.imply p q => match evalProp p, evalProp q with
    | PropForm.const false, _ => PropForm.const true
    | PropForm.const true,  x => x
    | p,           q          => PropForm.imply p q
  | PropForm.equiv p q => match evalProp p, evalProp q with
    | PropForm.const false, PropForm.const false => PropForm.const true
    | PropForm.const true,  PropForm.const true  => PropForm.const true
    | PropForm.const true,  PropForm.const false => PropForm.const false
    | PropForm.const false, PropForm.const true  => PropForm.const false
    | p,           q                             => PropForm.equiv p q

def substProp (subs : Nat → PropForm) : PropForm → PropForm
  | PropForm.const  c   => PropForm.const c
  | PropForm.var    v   => subs v
  | PropForm.not    p   => PropForm.not (substProp subs p)
  | PropForm.and    p q => PropForm.and (substProp subs p) (substProp subs q)
  | PropForm.or     p q => PropForm.or (substProp subs p) (substProp subs q)
  | PropForm.imply  p q => PropForm.imply (substProp subs p) (substProp subs q)
  | PropForm.equiv  p q => PropForm.equiv (substProp subs p) (substProp subs q)

def testNthVar (n : Nat) (p : PropForm) : PropForm :=
  PropForm.and (substProp (subsN n true) p) (substProp (subsN n false) p)
  where subsN (n : Nat) (b : Bool) : Nat → PropForm := λ x => if n == x then PropForm.const b
                                                                        else PropForm.var   x

-- Look at me, I am the theorem prover now.
#eval evalProp (testNthVar 0 (PropForm.or (PropForm.var 0) (PropForm.not (PropForm.var 0))))
#eval evalProp (testNthVar 1 (testNthVar 0 (PropForm.equiv
  (PropForm.imply (PropForm.var 0) (PropForm.var 1))
  (PropForm.or (PropForm.not (PropForm.var 0)) (PropForm.var 1)))))
#eval evalProp (testNthVar 1 (testNthVar 0 (PropForm.equiv
  (PropForm.imply (PropForm.var 0) (PropForm.var 1))
  (PropForm.or (PropForm.var 0) (PropForm.var 1)))))

end exercise_4
end chapter_7_exercises
