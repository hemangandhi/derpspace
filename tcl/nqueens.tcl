# Translated from OCaML since I'm a bit lazy.

proc abs { x } {
    expr { ($x < 0) ? -$x : $x }
}

proc check_queens { q1x q1y q2x q2y } {
    # TODO: tidy this up.
    set diag_x [abs [expr {$q2x - $q1x}]]
    set diag_y [abs [expr {$q2y - $q1y}]]
    expr { $q1x == $q2x || $q1y == $q2y || $diag_x == $diag_y }
}

proc extend_boards { boards n } {
    set next_boards ""
    foreach board $boards {
        set queen_col [llength $board]
        for {set queen_row 0} {$queen_row < $n} {incr queen_row} {
            set old_col 0
            set collides 0
            foreach old_row $board {
                if [check_queens $old_row $old_col $queen_row $queen_col] {
                    set collides 1
                    break
                }
                incr old_col
            }
            if [expr ! $collides] {
                lappend next_boards [concat board $queen_row]]
            }
        }
    }
    return $next_boards
}

proc n_queens { n } {
    set boards {}
    for {set i 0} {$i < $n} {incr i} {
        lappend queen_at_i $i
        lappend boards $queen_at_i
        set queen_at_i ""
    }
    puts $boards
    for {set i 0} {$i < $n} {incr i} {
        set boards [extend_boards $boards $n]
    }
    return $boards
}
