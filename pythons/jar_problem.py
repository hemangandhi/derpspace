#! /usr/bin/env python3

def indices_where(iter, pred):
    return map(lambda x: x[0], filter(lambda x: pred(x[1]), enumerate(iter)))

def all_perms(iter1, iter2):
    return ((i, j) for i in iter1 for j in iter2)

def moved_state(src, dest, curr_state, jar_caps):
    new = list(curr_state)
    move_amt = min(curr_state[src], jar_caps[dest] - curr_state[dest])
    new[src] -= move_amt
    new[dest] += move_amt
    return tuple(new)

def recursive_solve(jar_cap, jar_state, target, moves, move_ct):
    if any(i == target for i in jar_state):
        return moves
    else:
        possible_sources = list(indices_where(jar_state, lambda x: x != 0))
        possible_dests = list(indices_where(range(len(jar_state)), lambda x: jar_state[x] != jar_cap[x]))
        for (i, j) in filter(lambda p: p[0] != p[1], all_perms(possible_sources, possible_dests)):
            nm = moved_state(i, j, jar_state, jar_cap)
            if nm in moves: continue
            moves[nm] = (move_ct, i, j)
            v = recursive_solve(jar_cap, nm, target, moves, move_ct + 1)
            if v is not None:
                return v
        moves[jar_state] = (-1, 0, 0)
        return None

def solve_jars_for(jar_cap, jar_state, target):
    if len(jar_cap) == len(jar_state) and not target > max(jar_cap):
        sln = recursive_solve(jar_cap, jar_state, target, dict(), 1)
        if sln is not None:
            return map(lambda x: (x, sln[x]), filter(lambda st: sln[st][0] != -1, sorted(sln, key = lambda ent: sln[ent][0])))
    return None

def validate_input(prmpt, validator, on_invalid, converter = lambda x: x):
    v = input(prmpt + "> ")
    while not validator(v):
        print(on_invalid(v))
        v = input(prmpt + "> ")
    return converter(v)

def get_problem():
    jar_caps = validate_input("Enter the jar capacities separated by commas", 
                              lambda s: all(t.strip().isdigit() and int(t) > 0 for t in s.split(",")),
                              lambda inv: inv + " is not a comma-separated list of capacities",
                              lambda v: tuple(map(lambda t: int(t.strip()), v.split(","))))

    err = "no error"
    jar_states = None
    def validate_jar_state(st):
        nonlocal err, jar_states
        vals = st.split(",")
        if len(vals) != len(jar_caps):
            err = "incorrect number of jars"
            return False
        elif not all(t.strip().isdigit() and int(t) >= 0 for t in vals):
            err = "all jars must be filled with non-negative integer amounts"
            return False
        else:
            maybe_states = tuple(map(lambda x: int(x.strip()), vals))
            if any(maybe_states[i] > jar_caps[i] or maybe_states[i] < 0 for i in range(len(jar_caps))):
                err = "some jars overflow or are incorrectly filled"
                return False
            jar_states = maybe_states
            return True

    validate_input("Enter the jar states separated by commas", validate_jar_state, lambda s: "{0} has error: {1}.".format(s, err))
    target = validate_input("Enter the target", lambda s: s.isdigit(), lambda s: s + " is not a number!", lambda s: int(s))
    return (jar_caps, jar_states, target)

def print_state(jar_state, move_info):
    return "For move {0}, move liquid from jar {1} to jar {2}, yielding jars with: {3} {4} {5}".format(*(move_info + jar_state))

def one_sln():
    sln = solve_jars_for(*(get_problem()))
    if sln is not None:
        print("\n".join(map(lambda x: print_state(*x), sln)))
    else:
        print("No solution")

def main():
    print("Welcome the the jar problem solver!")
    v = 'y'
    while v == 'y':
        one_sln()
        v = input("Enter 'y' to solve another problem >")

if __name__ == "__main__": main()
