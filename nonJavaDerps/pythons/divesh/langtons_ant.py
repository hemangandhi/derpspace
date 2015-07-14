#implementation of Langton's ant. py3 due to print statements.

dir_vecs = ((1,0),(0,1),(-1,0),(0,-1))

def run_board(steps, size, board = None, ant = None, print_all = False, print_args = {"black":"#","white":" ","ant_p":"v>^<"}):
    """
    Simulates steps steps of langton's ant on a square board with side
    size.

    Ant is the ant's initial position and must be a list: [row (int), col (int), direction (int between 0 and 3)]

    print_all will print every step if set to True.

    The board may be passed in, at which point size is ignored. The board must be square and all booleans.

    print_args is blindly passed to print_board to handle prints. See documentation therein for details.

    The results are on a torus and rotated due to the nature of computers.
    """
    if board == None:
        board = [[True for i in range(size)] for j in range(size)]
    else:
        size = len(board)

    if ant == None:
        ant = [size//2,size//2,0]

    for i in range(steps):
        board[ant[0]][ant[1]] = not board[ant[0]][ant[1]]

        ant[0] += dir_vecs[ant[2]][0]
        ant[1] += dir_vecs[ant[2]][1]

        if ant[0] < 0:
            ant[0] = size - 1
        elif ant[0] >= size:
            ant[0] = 0

        if ant[1] < 0:
            ant[1] = size - 1
        elif ant[1] >= size:
            ant[1] = 0

        if board[ant[0]][ant[1]]:
            ant[2] = (ant[2] + 1) % 4
        else:
            ant[2] = (ant[2] + 3) % 4

        if print_all and i < steps - 1:
            print_board(board,ant,**print_args)

    print_board(board,ant,**print_args)        

def print_board(board, ant, black = "#", white = " ", ant_p = "v>^<"):
    """
    Prints the board state, given the ant as a list, specified in run_board.

    The optional arguments are printed in their places, the ant string indices correspond to direction.
    """
    print("".join("-" for i in board) + "--")
    for i in range(len(board)):
        st = "|"
        for j in range(len(board[i])):
            if ant[0] == i and ant[1] == j:
                st += ant_p[ant[2]]
            elif board[i][j]:
                st += white
            else:
                st += black
        print(st + "|")
    print("".join("-" for i in board) + "--")    

if __name__ == "__main__":
    run_board(11000,75)
