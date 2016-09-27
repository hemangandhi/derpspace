open List;;
(* The 8 queens problem!!! *)

let checking_queens (q1x, q1y) (q2x, q2y) = 
        q1x = q2x || q1y = q2y
        || abs (q2x - q1x) = abs (q2y - q1y)

let next_col_for_list lis rows = 
        let col_num = length lis in
        let rec inner_loop row accs = 
                if row = rows then accs
                else (
                  let new_pt = (col_num, row) in
                  if not (exists (checking_queens new_pt) lis) 
                  then inner_loop (row + 1) ((new_pt :: lis) :: accs)
                  else inner_loop (row + 1) accs)
        in inner_loop 0 []

let solve_queens dim = 
        let rec loop curr acc = 
                if curr = dim then acc 
                else loop (curr + 1) 
                          (fold_left append 
                                     []
                                     (map (fun ls -> next_col_for_list ls dim) acc))
        in loop 0 (next_col_for_list [] dim);;

let print_sol sol = 
        iter (fun (x, y) -> print_string "("; 
                            print_int x; 
                            print_string ", "; 
                            print_int y; 
                            print_string "), ") sol;
        print_string "\n";;

let sols = (solve_queens 1) in
        if 0 = length sols then print_string "No solutions!\n"
        else iter print_sol sols;;

iter print_sol (next_col_for_list [(0, 1)] 8)
