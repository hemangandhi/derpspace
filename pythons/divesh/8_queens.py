def intersect(x1,y1,x2,y2):
    return x1 == x2 or y1 == y2 or abs(x1 - x2) == abs(y1 - y2)

def solve_queens_h(curr_sol,ind):
    if ind == len(curr_sol):
        return curr_sol
    while curr_sol[ind] < len(curr_sol):
        if not any(intersect(ind,curr_sol[ind],i,curr_sol[i]) for i in range(ind)):
            guess = solve_queens_h(curr_sol[:],ind + 1)
            if guess:
                return guess
        curr_sol[ind] = curr_sol[ind] + 1
    return False    

def solve_queens(dim):
    return solve_queens_h([0 for i in range(dim)],0)

def print_sol(sol):
    if not sol:
        print("No solution")
        return
    print('--')
    for i in sol:
        ln = ['#' for j in range(len(sol))]
        ln[i] = 'Q'
        print(''.join(ln))

if __name__ == "__main__":
    print_sol(solve_queens(8))
