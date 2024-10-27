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


theorem mod_6_0_5 : (∃ x : Nat, (n = 6 * x) ∨ (n = 6 * x + 1) ∨
                                (n = 6 * x + 2) ∨ (n = 6 * x + 3) ∨
                                (n = 6 * x + 4) ∨ (n = 6 * x + 5)) :=
  Nat.recOn (motive := λ n => ∃ x : Nat, (n = 6 * x) ∨ (n = 6 * x + 1) ∨
                                (n = 6 * x + 2) ∨ (n = 6 * x + 3) ∨
                                (n = 6 * x + 4) ∨ (n = 6 * x + 5)) n
  (⟨0, Or.inl (eq_comm.mp (Nat.mul_zero 6))⟩)
  (fun (n : Nat) (ih: ∃ x : Nat, (n = 6 * x) ∨ (n = 6 * x + 1) ∨
                                (n = 6 * x + 2) ∨ (n = 6 * x + 3) ∨
                                (n = 6 * x + 4) ∨ (n = 6 * x + 5)) => by
    have ⟨x, mod6⟩ := ih
    match mod6 with
    | Or.inl mod6_0          => exact ⟨x, Or.inr (Or.inl (show Nat.succ n = 6 * x + 1 by rw [mod6_0, Nat.succ_eq_add_one]))⟩
    | Or.inr (Or.inl mod6_1) => exact ⟨x, Or.inr (Or.inr (Or.inl (show Nat.succ n = 6 * x + 2 by rw [mod6_1, Nat.succ_eq_add_one])))⟩
    | Or.inr (Or.inr (Or.inl mod6_2)) =>
      exact ⟨x, Or.inr (Or.inr (Or.inr (Or.inl (show Nat.succ n = 6 * x + 3 by rw [mod6_2, Nat.succ_eq_add_one]))))⟩
    | Or.inr (Or.inr (Or.inr (Or.inl mod6_3))) =>
      exact ⟨x, Or.inr (Or.inr (Or.inr (Or.inr (Or.inl (show Nat.succ n = 6 * x + 4 by rw [mod6_3, Nat.succ_eq_add_one])))))⟩
    | Or.inr (Or.inr (Or.inr (Or.inr (Or.inl mod6_4)))) =>
      exact ⟨x, Or.inr (Or.inr (Or.inr (Or.inr (Or.inr (show Nat.succ n = 6 * x + 5 by rw [mod6_4, Nat.succ_eq_add_one])))))⟩
    | Or.inr (Or.inr (Or.inr (Or.inr (Or.inr mod6_5)))) =>
      exact ⟨x + 1, Or.inl (show Nat.succ n = 6 * (x + 1) by rw [mod6_5, Nat.succ_eq_add_one, Nat.mul_succ])⟩)

theorem mult6_no_remainder : ¬ (6 ∣ n) ↔ ∃ x : Nat, (n = 6 * x + 1) ∨
                                  (n = 6 * x + 2) ∨ (n = 6 * x + 3) ∨
                                  (n = 6 * x + 4) ∨ (n = 6 * x + 5) := by
  apply Iff.intro
  . intro not_mult6
    have ⟨x, mod6⟩ := mod_6_0_5 n
    cases mod6 with
    | inl mod6_0 => exact absurd ⟨x, mod6_0⟩ not_mult6
    | inr mod6_12 => exact ⟨x, mod6_12⟩
  . intro ⟨x, mod6_15⟩
    apply byContradiction
    intro mod6_0
    have ⟨y, y_6_n⟩ := not_not.mp mod6_0
    -- What follows sucks, but I should learn how to show  (n = a + b) → (a ≤ n) to fix it.
    -- The inductive step is pretty weird in that case since you need n = a + b + 1 to work, but the
    -- inductive hypothesis can't help you since it's an =. If you restate this in terms of ≤ to get
    -- something that transitivity can help with, though, you get stuck with needing b > 0 and I have no
    -- idea how to proceed: not sure how to make 1 the base case or show that b = 0 is absurd.
    -- (And copy-paste was easier. Perhaps proof by vim macro is the latest great technique.)
    cases mod6_15 with
    | inl mod6_1 =>
      have xy_eq : 6 * y = 6 * x + 1 := y_6_n.symm.trans mod6_1
      have y6_succ_x6 : 6 * y = Nat.succ (6 * x) := by rw [xy_eq, ←Nat.succ_eq_add_one]
      have one_is_6yx : 6 * y - 6 * x = 1 := (
        Nat.sub_eq_iff_eq_add
          (show 6 * x ≤ 6 * y from le_of_le_of_eq (Nat.le_succ (6 * x)) y6_succ_x6.symm )
          ).mpr (show 6 * y = 1 + 6 * x by rw [xy_eq, Nat.add_comm])
      have three_div_one := (⟨y - x, (show 1 = 6 * (y - x) by rw [one_is_6yx.symm, Nat.mul_sub_left_distrib])⟩: 6 ∣ 1)
      exact absurd (Nat.le_of_dvd (show 0 < 1 by simp) three_div_one) (show ¬(6 ≤ 1) by simp)
    | inr mod6_25 =>
      cases mod6_25 with
      | inl mod6_2 =>
        have xy_eq : 6 * y = 6 * x + 2 := y_6_n.symm.trans mod6_2
        have y6_succ_x6 : 6 * y = Nat.succ (Nat.succ (6 * x)) := by rw [xy_eq, ←Nat.succ_eq_add_one]
        have one_is_6yx : 6 * y - 6 * x = 2 := (
          Nat.sub_eq_iff_eq_add
            (show 6 * x ≤ 6 * y from
              Nat.le_trans (Nat.le_succ (6 * x))
                (le_of_le_of_eq (Nat.le_succ (Nat.succ (6 * x))) y6_succ_x6.symm))
            ).mpr (show 6 * y = 2 + 6 * x by rw [xy_eq, Nat.add_comm])
        have three_div_one := (⟨y - x, (show 2 = 6 * (y - x) by rw [one_is_6yx.symm, Nat.mul_sub_left_distrib])⟩: 6 ∣ 2)
        exact absurd (Nat.le_of_dvd (show 0 < 2 by simp) three_div_one) (show ¬(6 ≤ 2) by simp)
      | inr mod6_35 =>
        cases mod6_35 with
        | inl mod6_3 =>
          have xy_eq : 6 * y = 6 * x + 3 := y_6_n.symm.trans mod6_3
          have y6_succ_x6 : 6 * y = Nat.succ (Nat.succ (Nat.succ (6 * x))) := by rw [xy_eq, ←Nat.succ_eq_add_one]
          have one_is_6yx : 6 * y - 6 * x = 3 := (
            Nat.sub_eq_iff_eq_add
              (show 6 * x ≤ 6 * y from
                Nat.le_trans (Nat.le_succ (6 * x))
                  (Nat.le_trans (Nat.le_succ (Nat.succ (6 * x)))
                    (le_of_le_of_eq (Nat.le_succ (Nat.succ (Nat.succ (6 * x)))) y6_succ_x6.symm)))
              ).mpr (show 6 * y = 3 + 6 * x by rw [xy_eq, Nat.add_comm])
          have three_div_one := (⟨y - x, (show 3 = 6 * (y - x) by rw [one_is_6yx.symm, Nat.mul_sub_left_distrib])⟩: 6 ∣ 3)
          exact absurd (Nat.le_of_dvd (show 0 < 3 by simp) three_div_one) (show ¬(6 ≤ 3) by simp)
        | inr mod6_45 =>
          cases mod6_45 with
          | inl mod6_4 =>
            have xy_eq : 6 * y = 6 * x + 4 := y_6_n.symm.trans mod6_4
            have y6_succ_x6 : 6 * y = Nat.succ (Nat.succ (Nat.succ (Nat.succ (6 * x)))) := by rw [xy_eq, ←Nat.succ_eq_add_one]
            have one_is_6yx : 6 * y - 6 * x = 4 := (
              Nat.sub_eq_iff_eq_add
                (show 6 * x ≤ 6 * y from
                  Nat.le_trans (Nat.le_succ (6 * x))
                    (Nat.le_trans (Nat.le_succ (Nat.succ (6 * x)))
                      (Nat.le_trans (Nat.le_succ (Nat.succ (Nat.succ (6 * x))))
                        (le_of_le_of_eq (Nat.le_succ (Nat.succ (Nat.succ (Nat.succ (6 * x))))) y6_succ_x6.symm))))
                ).mpr (show 6 * y = 4 + 6 * x by rw [xy_eq, Nat.add_comm])
            have three_div_one := (⟨y - x, (show 4 = 6 * (y - x) by rw [one_is_6yx.symm, Nat.mul_sub_left_distrib])⟩: 6 ∣ 4)
            exact absurd (Nat.le_of_dvd (show 0 < 4 by simp) three_div_one) (show ¬(6 ≤ 4) by simp)
          | inr mod6_5 =>
            have xy_eq : 6 * y = 6 * x + 5 := y_6_n.symm.trans mod6_5
            have y6_succ_x6 : 6 * y = Nat.succ (Nat.succ (Nat.succ (Nat.succ (Nat.succ (6 * x))))) := by rw [xy_eq, ←Nat.succ_eq_add_one]
            have one_is_6yx : 6 * y - 6 * x = 5 := (
              Nat.sub_eq_iff_eq_add
                (show 6 * x ≤ 6 * y from
                  Nat.le_trans (Nat.le_succ (6 * x))
                    (Nat.le_trans (Nat.le_succ (Nat.succ (6 * x)))
                      (Nat.le_trans (Nat.le_succ (Nat.succ (Nat.succ (6 * x))))
                        (Nat.le_trans (Nat.le_succ (Nat.succ (Nat.succ (Nat.succ (6 * x)))))
                          (le_of_le_of_eq (Nat.le_succ (Nat.succ (Nat.succ (Nat.succ (Nat.succ (6 * x)))))) y6_succ_x6.symm)))))
                ).mpr (show 6 * y = 5 + 6 * x by rw [xy_eq, Nat.add_comm])
            have three_div_one := (⟨y - x, (show 5 = 6 * (y - x) by rw [one_is_6yx.symm, Nat.mul_sub_left_distrib])⟩: 6 ∣ 5)
            exact absurd (Nat.le_of_dvd (show 0 < 5 by simp) three_div_one) (show ¬(6 ≤ 5) by simp)

theorem square_free_6 : 6 ∣ a * a → 6 ∣ a := by
  intro dvd_sq_6
  apply byContradiction; intro ndvd_6_a
  have ⟨x, x6_r⟩ : ∃ x : Nat, (a = 6 * x + 1) ∨ (a = 6 * x + 2) ∨ (a = 6 * x + 3) ∨
                              (a = 6 * x + 4) ∨ (a = 6 * x + 5) := (mult6_no_remainder a).mp ndvd_6_a
  apply absurd dvd_sq_6
  apply (mult6_no_remainder (a * a)).mpr
  exact match x6_r with
  | Or.inl r_1 => ⟨
      (x * (6 * x) + x + x),
      Or.inl (
        show a * a = 6 * (x * (6 * x) + x + x) + 1 by
        calc a * a
        _ = (6 * x + 1) * (6 * x + 1)             := by rw [r_1]
        _ = 6 * x * (6 * x) + 6 * x + (6 * x + 1) := by rw [
          Nat.left_distrib, Nat.right_distrib, Nat.right_distrib, Nat.one_mul, Nat.one_mul, Nat.mul_one]
        _ = 6 * (x * (6 * x) + x + x) + 1         := by rw [
          Nat.mul_assoc, ←Nat.left_distrib, ←Nat.add_assoc, ←Nat.left_distrib])⟩
  | Or.inr (Or.inl r_2) => ⟨
      (x * (6 * x) + x * 2 + x * 2),
      Or.inr (Or.inr (Or.inr (Or.inl (
        show a * a = 6 * (x * (6 * x) + x * 2 + x * 2) + 2 * 2 by
        calc a * a
        _ = (6 * x + 2) * (6 * x + 2)                               := by rw [r_2]
        _ = 6 * x * (6 * x) + 2 * (6 * x) + (6 * x * 2 + 2 * 2)     := by rw [
          Nat.left_distrib, Nat.right_distrib, Nat.right_distrib]
        _ = 6 * (x * (6 * x)) + 6 * (x * 2) + (6 * (x * 2) + 2 * 2) := by rw [
          Nat.mul_assoc, Nat.mul_assoc, Nat.mul_comm 2 (6 * x), Nat.mul_assoc]
        _ = 6 * (x * (6 * x) + x * 2 + x * 2) + 2 * 2               := by rw [
          ←Nat.left_distrib, ←Nat.add_assoc, ←Nat.left_distrib]))))⟩
  | Or.inr (Or.inr (Or.inl r_3)) => ⟨
      (x * (6 * x) + x * 3 + x * 3 + 1),
      Or.inr (Or.inr (Or.inl (
        show a * a = 6 * (x * (6 * x) + x * 3 + x * 3 + 1) + 3 by
        calc a * a
        _ = (6 * x + 3) * (6 * x + 3)                               := by rw [r_3]
        _ = 6 * x * (6 * x) + 3 * (6 * x) + (6 * x * 3 + 3 * 3)     := by rw [
          Nat.left_distrib, Nat.right_distrib, Nat.right_distrib]
        _ = 6 * (x * (6 * x)) + 6 * (x * 3) + (6 * (x * 3) + 3 * 3) := by rw [
          Nat.mul_assoc, Nat.mul_assoc, Nat.mul_comm 3 (6 * x), Nat.mul_assoc]
        _ = 6 * (x * (6 * x) + x * 3 + x * 3) + 3 * 3               := by rw [
          ←Nat.left_distrib, ←Nat.add_assoc, ←Nat.left_distrib]
        _ = 6 * (x * (6 * x) + x * 3 + x * 3) + 6 * 1 + 3           := by simp
        _ = 6 * (x * (6 * x) + x * 3 + x * 3 + 1) + 3               := by rw [←Nat.left_distrib])))⟩
  | Or.inr (Or.inr (Or.inr (Or.inl r_4))) => ⟨
      (x * (6 * x) + x * 4 + x * 4 + 2),
      Or.inr (Or.inr (Or.inr (Or.inl (
        show a * a = 6 * (x * (6 * x) + x * 4 + x * 4 + 2) + 4 by
        calc a * a
        _ = (6 * x + 4) * (6 * x + 4)                               := by rw[r_4]
        _ = 6 * x * (6 * x) + 4 * (6 * x) + (6 * x * 4 + 4 * 4)     := by rw [
          Nat.left_distrib, Nat.right_distrib, Nat.right_distrib]
        _ = 6 * (x * (6 * x)) + 6 * (x * 4) + (6 * (x * 4) + 4 * 4) := by rw [
          Nat.mul_assoc, Nat.mul_assoc, Nat.mul_comm 4 (6 * x), Nat.mul_assoc]
        _ = 6 * (x * (6 * x) + x * 4 + x * 4) + 4 * 4               := by rw [
          ←Nat.left_distrib, ←Nat.add_assoc, ←Nat.left_distrib]
        _ = 6 * (x * (6 * x) + x * 4 + x * 4) + 6 * 2 + 4           := by simp
        _ = 6 * (x * (6 * x) + x * 4 + x * 4 + 2) + 4               := by rw [←Nat.left_distrib]))))⟩
  | Or.inr (Or.inr (Or.inr (Or.inr r_5))) => ⟨
      (x * (6 * x) + x * 5 + x * 5 + 4),
      Or.inl (
        show a * a = 6 * (x * (6 * x) + x * 5 + x * 5 + 4) + 1 by
        calc a * a
        _ = (6 * x + 5) * (6 * x + 5)                               := by rw[r_5]
        _ = 6 * x * (6 * x) + 5 * (6 * x) + (6 * x * 5 + 5 * 5)     := by rw [
          Nat.left_distrib, Nat.right_distrib, Nat.right_distrib]
        _ = 6 * (x * (6 * x)) + 6 * (x * 5) + (6 * (x * 5) + 5 * 5) := by rw [
          Nat.mul_assoc, Nat.mul_assoc, Nat.mul_comm 5 (6 * x), Nat.mul_assoc]
        _ = 6 * (x * (6 * x) + x * 5 + x * 5) + 5 * 5               := by rw [
          ←Nat.left_distrib, ←Nat.add_assoc, ←Nat.left_distrib]
        _ = 6 * (x * (6 * x) + x * 5 + x * 5) + 6 * 4 + 1           := by simp
        _ = 6 * (x * (6 * x) + x * 5 + x * 5 + 4) + 1               := by rw [←Nat.left_distrib])⟩

theorem sqrt_6_irrational_coprime: (a.gcd b = 1) → (a * a ≠ 6 * (b * b)) := by
  intro coprimes
  apply byContradiction
  intro nab6
  have eq: (a * a =  6 * (b * b)) := not_not.mp nab6
  have a_is_mult_6: 6 ∣ a := square_free_6 a ⟨b * b, eq⟩
  have ⟨a_div6, a_is_6_a_div6⟩ := a_is_mult_6
  have substitute_a: 6 * (6 * (a_div6 * a_div6)) = 6 * (b * b) := by rw[eq.symm, a_is_6_a_div6,
    Nat.mul_assoc, ←Nat.mul_assoc a_div6 _, Nat.mul_comm a_div6 6, Nat.mul_assoc]
  have b_is_mult_6: 6 ∣ b := (square_free_6 b ⟨a_div6 * a_div6,
    ((Nat.mul_left_cancel_iff (show 0 < 6 by simp) (6 * (a_div6 * a_div6)) (b * b)).mp substitute_a).symm⟩)
  have dvd_6_gcd := Nat.dvd_gcd a_is_mult_6 b_is_mult_6
  have three_div_one : 6 ∣ 1 := coprimes ▸ dvd_6_gcd
  exact absurd (Nat.le_of_dvd (show 0 < 1 by simp) three_div_one) (show ¬(6 ≤ 1) by simp)


end exercise_1_2_1

section exercise_1_2_2

open Classical

variable (a b n : Nat)

-- OK, I thought I could do something fancy with "suppose a = 0 but we know 1 ≤ a, so absurd."
-- But one_le_iff_ne_zero is in mathlib, so may be later.
theorem pow_2_odd : (1 ≤ a) → ∃ x : N, 2 ^ a = 2 * n := by
  intro a_ge_1
  cases a with
  | zero => exact absurd rfl ((Nat.one_le_iff_ne_zero 0).mp a_ge_1)
  | succ n => exact ⟨2 ^ n, Nat.pow_succ' 2 n⟩

end exercise_1_2_2

section exercise_1_2_3

-- TODO: figure out how to have these be types and do this in something that's actually
-- kind of about sets?
variable (α β γ : Prop)

theorem part_d : α ∧ β ∧ γ ↔ α ∧ (β ∧ γ) := by
  apply Iff.intro
  . intro ⟨a, b, c⟩
    exact ⟨a, ⟨b, c⟩⟩
  . intro ⟨a, ⟨b, c⟩⟩
    exact ⟨a, b, c⟩


theorem part_e : α ∧ (β ∨ γ) ↔ (α ∧ β) ∨ (α ∧ γ) := by
  apply Iff.intro
  . intro ⟨a, b_or_c⟩
    cases b_or_c with
    | inl b =>  exact Or.inl ⟨a, b⟩
    | inr c =>  exact Or.inr ⟨a, c⟩
  . intro a_and_b_or_a_and_c
    match a_and_b_or_a_and_c with
    | Or.inl ⟨a, b⟩ => exact ⟨a, Or.inl b⟩
    | Or.inr ⟨a, c⟩ => exact ⟨a, Or.inr c⟩



end exercise_1_2_3
