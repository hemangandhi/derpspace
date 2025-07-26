# From https://wiki.tcl-lang.org/page/Closures (Todd Conram's comment.)

proc curry {new args} {
    uplevel [list interp alias {} $new {}] $args
}

# Create a proc named 'name' that will create a closure over the supplied
# 'variables' definition for the 'lambda_proc'. Any supplied variables in
# 'arglist' can be used to initialize 'variables' during the closure
# definition.
proc make-closure-proc {name arglist variables lambda_proc} {
    set invoke_context [uplevel 1 {namespace current}]
    set name_context ${invoke_context}::$name

    # Create a namespace called $name_context to hold auto_cnt
    #
    namespace eval $name_context {
       if {![info exists auto_cnt]} { variable auto_cnt -1}
    }

    # Now, build a proc in invocation context that will create
    # closures. We do this by substituting all of the passed
    # parameters (name, arglist, variables, lambda_proc) and the
    # $name_context.
    #
    # The resulting proc will:
    # 1. Accept $arglist as initializers for the closures.
    # 2. Create a unique closure_name from the auto_cnt variable.
    # 3. Create a namespace for the closure.
    # 4. Evaluate the $variables (optionally evaluating them with
    #    $arglist).
    # 5. Create an alias called 'dispatch' for the lambda_proc.
    # 6. Return the alias.
    #
    namespace eval $invoke_context \
       [subst -nocommands -nobackslashes {
           proc $name {$arglist} {
               set closure_name \
               ${name_context}::$name[incr ${name_context}::auto_cnt]
               eval [subst {
                   namespace eval [set closure_name] {
                       $variables
                   }
               }]
               namespace eval [set closure_name] {
                   # Curry a dispatcher for the lambda_proc.
                   curry [namespace current]::dispatch [$lambda_proc]
               }
               return [set closure_name]::dispatch
           }
       }]
}

proc delete-closure {name} {
    namespace delete [namespace qualifiers $name]
}

proc lambda {arglst body} {
    set level [info level 0]
    set name [string map {\n _ \t _ \" _ " " _ \; _ $ _ : _ \{ _ \} _ \[ _ \] _} $level]
    set invoke_context [uplevel namespace current]
    proc ${invoke_context}::$name $arglst $body
    return ${invoke_context}::$name
}

# That's what's needed to add closures?
# TCL arrays suck, so this just implements them as cons cells.

make-closure-proc cons {_first _second} {
    variable first $_first
    variable second $_second
} {
    lambda {app} {
        variable first
        variable second
        return [$app $first $second]
    }
}


proc car { cons_cell } {
    proc fst { l r } {
        return $l
    }
    $cons_cell fst
}

proc cdr { cons_cell } {
    proc snd { l r } {
        return $r
    }
    $cons_cell snd
}

# TODO: how to find the end of the list?
puts [info proc [cdr [cons 2 [cons 3 4]]]]
puts [info proc [cdr [cons 2 3]]]
puts [info proc [cons 2 3]]
puts [info proc cons]
set x [cdr [cons 2 [cons 3 4]]]
puts [info vars $x]

# TODO: finish this implementation when we know how to detect the end.
proc list_map {cons_list map_fn } {
    proc apply-map { l r } {
        return [cons [$map_fn $l] [list_map $r $map_fn]]
    }
    apply-map
}
