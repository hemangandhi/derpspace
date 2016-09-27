let factorial n = 
        let rec loop ct acc = 
                if ct = 0 then acc
                else loop (ct - 1) (ct * acc)
        in loop n 1;;

print_endline (string_of_int (factorial 5))
