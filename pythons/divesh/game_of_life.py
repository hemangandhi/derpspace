#game of life

def run(steps,init_board, spawn = 3, survive = [2,3], print_pause = True):
    """
    Runs steps generations of Conway's game of life. init_board is the initial configuration.
    """
    print_board(init_board)
    
    for i in range(steps):
        next_g = [[((neighbours(j,k,init_board) in survive) and init_board[k][j] or neighbours(j,k,init_board) == spawn) for j in range(len(init_board[k]))] for k in range(len(init_board))]
        init_board = next_g

        if print_pause and i < steps - 1:
            print_board(init_board)
            input("Press enter to continue > ")

    print_board(init_board)        
            

def neighbours(x,y,board):
    """
    Gets the live neighbour count of a cell on the board.
    """
    count = 0

    for i in (-1,0,1):
        for j in (-1,0,1):
            if i == 0 and j == 0:
                continue

            if x + i < 0:
                xa = len(board[0]) - 1
            elif x + i >= len(board[0]):
                xa = 0
            else:
                xa = x + i

            if y + j < 0:
                ya = len(board) - 1
            elif y + j >= len(board):
                ya = 0
            else:
                ya = y + j

            count += board[ya][xa]

    return count

def print_board(board, alive = "#", dead = " "):
    """
    Prints the board state, given the state as a list, specified in run_board.
    """
    print("+" + "".join("-" for i in board[0]) + "+")
    for i in range(len(board)):
        st = "|"
        for j in range(len(board[i])):
            if board[i][j]:
                st += alive
            else:
                st += dead
        print(st + "|")
    print("+" + "".join("-" for i in board[0]) + "+")

def add_pattern(pat, leng, wid, row, col):
    """
    Transposes pat into a all-dead matrix of length leng and width wid with the top left of pat
    at row and col specified.
    """
    return [[0 <= i - row < len(pat) and 0 <= j - col < len(pat[0]) and pat[i - row][j - col] for j in range(wid)] for i in range(leng)]

    
if __name__ == "__main__":
    glider = [[False,True,False],
              [False,False,True],
              [True,True,True]]
    
    run(200,add_pattern(glider,10,10,4,4))

                
