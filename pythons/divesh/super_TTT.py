#! /usr/bin/env python3

def won_board(board):
    v = filter(lambda x: x[0] != "e" and all(x[0] == j for j in x), board)
    try:
        homog_row = next(v)
        return homog_row[0]
    except StopIteration:
        v = filter(lambda x: x[0] != "e" and all(x[0] == j for j in x), 
                (list(map(lambda x: x[i], board)) for i in range(len(board))))
        try:
            homog_col = next(v)
            return homog_col[0]
        except StopIteration:
            if all(board[i//3][i%3] == board[0][0] for i in range(0, 9, 4)) and board[0][0] != "e":
                return board[0][0]
            elif all(board[i//3][i%3] == board[0][0] for i in range(2, 7, 2)) and board[0][2] != "e":
                return board[0][2]
            else:
                return "e"

def won_outer(won_bds):
    return won_board([[won_bds[(i, j)] if (i, j) in won_bds else 'e' for j in range(3)]
                      for i in range(3)])

def init_game():
    moves = {(1, 1, i, j) for i in range(3) for j in range(3)}
    board = [[[["e" for i in range(3)] for j in range(3)] for k in range(3)] for l in range(3)]
    turn = "O"
    won_bds = dict()
    return (moves, board, turn, won_bds)

def char_to_p(moves, char, pt):
    if char == 'e' and pt in moves:
        return "*"
    elif char == 'e':
        return ' '
    else:
        return char

def inner_to_str(i_board, moves, won_bds, i, j):
    if (i, j) in won_bds and not any((i, j, k, l) in moves for k in range(3) for l in range(3)):
        return [won_bds[(i, j)] + " has won",
                "this one.",
                " Hooray! "]
    else:
        return [' | '.join(char_to_p(moves, i_board[k][l], (i, j, k, l)) for l in range(len(i_board[k])))
                for k in range(len(i_board))]

def print_state(moves, board, turn, won_bds):
    ins = [[inner_to_str(board[i][j], moves, won_bds, i, j) for j in range(len(board[i]))]
            for i in range(len(board))]
    print("The board:")
    print("Note that possible moves have been marked with '*'.")
    print("If a board is won and there is no possible move in it, the winner is indicated.")
    print("  " + (" "*4).join((" "*3).join(str(i * 3 + j) for j in range(3)) for i in range(3)))
    mk_row = lambda i, j: str(i*3 + j) + ' ' + ' || '.join(ins[i][k][j] for k in range(3))
    print(('\n ' + 37*'=' + '\n').join(('\n ' + 37*'-' + '\n').join(mk_row(i, j) for j in range(3)) for i in range(3)))
    print("\nIt's now {t}'s turn!".format(t = turn))

def get_input(moves):
    def to_4_ple(inp):
        r = int(inp[0])
        c = int(inp[-1])
        return (r//3, c//3, r % 3, c % 3)

    def is_valid(inp):
        if not inp[0].isdigit() or not inp[-1].isdigit():
            return False
        return to_4_ple(inp) in moves

    print("Please enter your move:")
    print("-> enter the number for the row followed by the number for the column,")
    print("-> note that the first and last characters inputted are used as row and column indices.")
    v = input("> ")
    while not is_valid(v):
        print("That was not a legal move...")
        v = input("> ")
    return to_4_ple(v)

def make_move(board, move, turn, won_bds):
    board[move[0]][move[1]][move[2]][move[3]] = turn
    if turn == 'X':
        turn = 'O'
    else:
        turn = 'X'

    if (move[2], move[3]) in won_bds:
        moves = {(i, j, k, l) for i in range(3) for j in range(3) for k in range(3) for l in range(3) if board[i][j][k][l] == 'e'}
    else:
        moves = {(move[2], move[3], i, j) for i in range(3) for j in range(3) if board[move[2]][move[3]][i][j] == 'e'}

    bv = won_board(board[move[0]][move[1]])
    if bv != 'e':
        won_bds[(move[0], move[1])] = bv

    return (turn, moves)

def run():
    print("Welcome! Have fun!")
    vs = list(init_game())
    while won_outer(vs[3]) == 'e':
        print_state(*vs) #moves, board, turn, won_bds
        mv = make_move(vs[1], get_input(vs[0]), vs[2], vs[3])
        vs[0] = mv[1]
        vs[2] = mv[0]
    print(won_outer(vs[3]) + " won the game!")

if __name__ == "__main__":
    run()
