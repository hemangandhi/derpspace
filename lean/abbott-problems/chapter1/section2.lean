section exercise_1_2_1

open Classical
attribute [simp] not_not

variable (a b n : Nat)

theorem mod_3_012 :  (∃ x : Nat, (n = 3 * x) ∨ (n = 3 * x + 1) ∨ (n = 3 * x + 2)) :=
  Nat.recOn (motive := λ n => ∃ x : Nat, (n = 3 * x) ∨ (n = 3 * x + 1) ∨ (n = 3 * x + 2)) n
  (⟨0, Or.inl (eq_comm.mp (Nat.mul_zero 3))⟩)
  (fun (n : Nat) (ih: ∃ x : Nat, (n = 3 * x) ∨ (n = 3 * x + 1) ∨ (n = 3 * x + 2)) => by
    have ⟨x, mod3⟩ := ih
    match mod3 with
    | Or.inl mod3_0          => exact ⟨x, Or.inr (Or.inl (show Nat.succ n = 3 * x + 1 by rw [mod3_0, Nat.succ_eq_add_one]))⟩
    | Or.inr (Or.inl mod3_1) => exact ⟨x, Or.inr (Or.inr (show Nat.succ n = 3 * x + 2 by rw [mod3_1, Nat.succ_eq_add_one]))⟩
    | Or.inr (Or.inr mod3_2) => exact ⟨x + 1, Or.inl (show Nat.succ n = 3 * (x + 1) by rw [mod3_2, Nat.succ_eq_add_one, Nat.mul_succ])⟩)

theorem mult3_no_remainder : ¬ (3 ∣ n) ↔ ∃ x : Nat, n = 3 * x + 1 ∨ n = 3 * x + 2 := by
  apply Iff.intro
  . intro not_mult3
    have ⟨x, mod3⟩ := mod_3_012 n
    cases mod3 with
    | inl mod3_0 => exact absurd ⟨x, mod3_0⟩ not_mult3
    | inr mod3_12 => exact ⟨x, mod3_12⟩
  . intro ⟨x, mod3_12⟩
    apply byContradiction
    intro mod3_0
    have ⟨y, y_3_n⟩ := not_not.mp mod3_0
    cases mod3_12 with
    | inl mod3_1 =>
      have xy_eq : 3 * y = 3 * x + 1 := y_3_n.symm.trans mod3_1
      have y3_succ_x3 : 3 * y = Nat.succ (3 * x) := by rw [xy_eq, ←Nat.succ_eq_add_one]
      have one_is_3yx : 3 * y - 3 * x = 1 := (
        Nat.sub_eq_iff_eq_add
          (show 3 * x ≤ 3 * y from le_of_le_of_eq (Nat.le_succ (3 * x)) y3_succ_x3.symm )
          ).mpr (show 3 * y = 1 + 3 * x by rw [xy_eq, Nat.add_comm])
      have three_div_one := (⟨y - x, (show 1 = 3 * (y - x) by rw [one_is_3yx.symm, Nat.mul_sub_left_distrib])⟩: 3 ∣ 1)
      exact absurd (Nat.le_of_dvd (show 0 < 1 by simp) three_div_one) (show ¬(3 ≤ 1) by simp)
    | inr mod3_2 =>
      have xy_eq : 3 * y = 3 * x + 2 := y_3_n.symm.trans mod3_2
      have y3_succ_x3 : 3 * y = Nat.succ (Nat.succ (3 * x)) := by rw [xy_eq, ←Nat.succ_eq_add_one]
      have two_is_3yx : 3 * y - 3 * x = 2 := (
        Nat.sub_eq_iff_eq_add
          (show 3 * x ≤ 3 * y from
            Nat.le_trans (Nat.le_succ (3 * x))
              (le_of_le_of_eq (Nat.le_succ (Nat.succ (3 * x))) y3_succ_x3.symm))
          ).mpr (show 3 * y = 2 + 3 * x by rw [xy_eq, Nat.add_comm])
      have three_div_two := (⟨y - x, (show 2 = 3 * (y - x) by rw [two_is_3yx.symm, Nat.mul_sub_left_distrib])⟩: 3 ∣ 2)
      exact absurd (Nat.le_of_dvd (show 0 < 2 by simp) three_div_two) (show ¬(3 ≤ 2) by simp)

theorem primality_ish_of_3 : 3 ∣ a * b → (3 ∣ a ∨ 3 ∣ b) := by
  intro div_3_ab
  apply byContradiction
  intro neither_3a_3b
  have ⟨not_div_3_a, not_div_3_b⟩ : (¬ 3 ∣ a) ∧ (¬ 3 ∣ b) := not_or.mp neither_3a_3b
  have ⟨x_a, remainder_a⟩ : ∃ x : Nat, a = 3 * x + 1 ∨ a = 3 * x + 2 := (mult3_no_remainder a).mp not_div_3_a
  have ⟨x_b, remainder_b⟩ : ∃ x : Nat, b = 3 * x + 1 ∨ b = 3 * x + 2 := (mult3_no_remainder b).mp not_div_3_b
  apply absurd div_3_ab
  apply (mult3_no_remainder (a * b)).mpr
  exact match remainder_a, remainder_b with
  | Or.inl a_1, Or.inl b_1  => ⟨
    x_a * 3 * x_b + x_b + x_a,
    Or.inl (show a * b = 3 * (x_a * 3 * x_b +  x_b + x_a) + 1 by
            calc a * b
            _ = (3 * x_a + 1) * ( 3 * x_b + 1)                := by rw [a_1, b_1]
            _ = 3 * x_a * (3 * x_b) + 3 * x_b + (3 * x_a + 1) := by rw [
              Nat.left_distrib, Nat.right_distrib, Nat.right_distrib,
              Nat.mul_one, Nat.mul_one, Nat.one_mul]
            _ = 3 * (x_a * 3 * x_b +  x_b + x_a) + 1          := by rw [
              Nat.mul_assoc, ←Nat.left_distrib, ←Nat.add_assoc, ←Nat.left_distrib, ← Nat.mul_assoc])⟩
  | Or.inl a_1, Or.inr b_2  => ⟨
    x_a * 3 * x_b + x_b + x_a * 2,
    Or.inr (show a * b = 3 * (x_a * 3 * x_b +  x_b + x_a * 2) + 2 by
            calc a * b
            _ = (3 * x_a + 1) * ( 3 * x_b + 2)                    := by rw [a_1, b_2]
            _ = 3 * x_a * (3 * x_b) + 3 * x_b + (3 * x_a * 2 + 2) := by rw [
              Nat.left_distrib, Nat.right_distrib, Nat.right_distrib,
              Nat.one_mul, Nat.one_mul]
            _ = 3 * (x_a * 3 * x_b +  x_b + x_a * 2) + 2          := by rw [
              Nat.mul_assoc, ←Nat.left_distrib, ←Nat.add_assoc, Nat.mul_assoc, ←Nat.left_distrib, ← Nat.mul_assoc])⟩
  | Or.inr a_2, Or.inl b_1  => ⟨
    x_a * 3 * x_b + x_b * 2 + x_a,
    Or.inr (show a * b = 3 * (x_a * 3 * x_b +  x_b * 2 + x_a) + 2 by
            calc a * b
            _ = (3 * x_a + 2) * ( 3 * x_b + 1)                      := by rw [a_2, b_1]
            _ = 3 * x_a * (3 * x_b) + 2 * (3 * x_b) + (3 * x_a + 2) := by rw [
              Nat.left_distrib, Nat.right_distrib, Nat.right_distrib,
              Nat.mul_one, Nat.mul_one]
            _ = 3 * (x_a * 3 * x_b +  x_b * 2 + x_a) + 2            := by rw [
              Nat.mul_assoc, Nat.mul_assoc, Nat.mul_comm 2 _, Nat.mul_assoc,
              ←Nat.left_distrib, ←Nat.add_assoc, ←Nat.left_distrib, ← Nat.mul_assoc])⟩
  | Or.inr a_2, Or.inr b_2  => ⟨
    x_a * 3 * x_b + x_b * 2 + x_a * 2 + 1,
    Or.inl (show a * b = 3 * (x_a * 3 * x_b +  x_b * 2 + x_a * 2 + 1) + 1 by
            calc a * b
            _ = (3 * x_a + 2) * ( 3 * x_b + 2)                                := by rw [a_2, b_2]
            _ = 3 * (x_a * (3 * x_b)) + 3 * x_b * 2 + (3 * (x_a * 2) + 2 * 2) := by rw [
              Nat.left_distrib, Nat.right_distrib, Nat.right_distrib, Nat.mul_assoc, Nat.mul_assoc, Nat.mul_comm 2 _]
            _ = 3 * (x_a * 3 * x_b +  x_b * 2 + x_a * 2) + 2 * 2              := by rw [
              Nat.mul_assoc, ←Nat.left_distrib, ←Nat.add_assoc, ←Nat.left_distrib, ← Nat.mul_assoc]
            _ = 3 * (x_a * 3 * x_b +  x_b * 2 + x_a * 2) + 3 * 1 + 1          := by simp
            _ = 3 * (x_a * 3 * x_b +  x_b * 2 + x_a * 2 + 1) + 1              := by rw [←Nat.left_distrib])⟩


theorem sqrt_3_irrational_coprime: (a.gcd b = 1) → (a * a ≠ 3 * (b * b)) := by
  intro coprimes
  apply byContradiction
  intro nab3
  have eq: (a * a =  3 * (b * b)) := not_not.mp nab3
  have a_is_mult_3: 3 ∣ a := or_self_iff.mp (primality_ish_of_3 a a ⟨b * b, eq⟩)
  have ⟨a_div3, a_is_3_a_div3⟩ := a_is_mult_3
  have substitute_a: 3 * (3 * (a_div3 * a_div3)) = 3 * (b * b) := by rw[eq.symm, a_is_3_a_div3,
    Nat.mul_assoc, ←Nat.mul_assoc a_div3 _, Nat.mul_comm a_div3 3, Nat.mul_assoc]
  have b_is_mult_3: 3 ∣ b := or_self_iff.mp (
    primality_ish_of_3 b b ⟨
      a_div3 * a_div3,
      ((Nat.mul_left_cancel_iff (show 0 < 3 by simp) (3 * (a_div3 * a_div3)) (b * b)).mp substitute_a).symm⟩)
  have dvd_3_gcd := Nat.dvd_gcd a_is_mult_3 b_is_mult_3
  have three_div_one : 3 ∣ 1 := coprimes ▸ dvd_3_gcd
  exact absurd (Nat.le_of_dvd (show 0 < 1 by simp) three_div_one) (show ¬(3 ≤ 1) by simp)

end exercise_1_2_1
