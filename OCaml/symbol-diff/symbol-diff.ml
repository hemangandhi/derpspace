
let is_int va =
        try ignore (int_of_string va); true
        with _ -> false;;

type expr = 
        | Sum of expr * expr
        | Prod of expr * expr
        | Quot of expr * expr
        | Diff of expr * expr
        | Var of string
        | Val of int;;

let rec expr_to_str = function
          Var a -> a
        | Val i -> string_of_int i
        | Sum (l, r) -> "(" ^ (expr_to_str l) ^ " + " ^ (expr_to_str r) ^ ")"
        | Prod (l, r) -> "(" ^ (expr_to_str l) ^ " * " ^ (expr_to_str r) ^ ")"
        | Diff (l, r) -> "(" ^ (expr_to_str l) ^ " - " ^ (expr_to_str r) ^ ")"
        | Quot (l, r) -> "(" ^ (expr_to_str l) ^ " / " ^ (expr_to_str r) ^ ")"

let rec diff_expr var = function
          Var a -> if a = var then Val 1 else Var a
        | Val v -> Val 0
        | Sum (l, r) -> Sum ((diff_expr var l), (diff_expr var r))
        | Prod (l, r) -> Sum ((Prod ((diff_expr var l), r)), (Prod ((diff_expr var r), l)))
        | Diff (l, r) -> Diff ((diff_expr var l), (diff_expr var r))
        | Quot (l, r) -> Quot ((Diff ((Prod (r, (diff_expr var l))), (Prod (l, (diff_expr var r))))), (Prod (r, r)))


