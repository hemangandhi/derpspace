#! /usr/bin/env python3

class Emotes:
    def __init__(self, blank, pawn, rook, knight, bishop, king, queen):
        self.blank = blank
        self.pawn = pawn
        self.rook = rook
        self.knight = knight
        self.bishop = bishop
        self.king = king
        self.queen = queen

def validated_input(prompt, get_error=lambda x: None, converter=lambda x: x):
    p = input(prompt)
    v = validator(p)
    while v is not None:
        print(v)
        p = input(prompt)
        v = validator(p)
    return converter(p)


class InputModule:
    def __init__(self, emotes):
        self.emotes = emotes
    def get_move(color, board):
        print("Enter the piece and then the x and y coordinate of a possible move")
        pieces = (p for r in board for p in r if p and p.black == color)
        moves = {piece: [(idx, x, y) for x, y in piece.get_moves(board)]\
                for idx, piece in enumerate(pieces)}
        for piece, move in moves:
            print("Move {}: " +\
                    " or ".join("{},{},{}".format(*move) for move in sorted(moves)))
        options = set(moves.values())
        tpl = lambda p: tuple(map(int, p.split(",")))
        user_move = validated_input(prompt,
                lambda p: {
            True: None,
            False: "Enter one of the options above"
            }[all(q.is_digit() for q in p) and tpl(p) in options],
                tpl)

# TODO: interface for castling and promotions
def board_factory(emotes, input_module_maker):

    class Space:
        def __init__(self, x, y):
            self.coord = (x, y)
        def __eq__(self, other):
            return self.coord == other.coord
        def __str__(self):
            return emotes.blank[self.coords[0] % 2 != self.coords[1] % 2]
        def __bool__(self):
            return False
        def __hash__(self):
            return hash(self.coord)

    class Piece(Space):
        def __init__(self, x, y, is_white):
            self.black = not is_white
            self.moved = False
            Space.__init__(self, x, y)
        def __eq__(self, other):
            return self.coord == other.coord and self.black == other.black and self.moved == other.moved
        def __bool__(self):
            return True
        def moves(self, board):
            raise NotImplemented("swubcwass me pwease UwU")
        def move(self, board, new_coords):
            if new_coords not in set(self.moves(board)):
                raise ValueError("Cannot make this move!")
            board[self.coords[0]][self.coords[1]] = Space(self.coords[0], self.coords[1])
            board[new_coords[0]][new_coords[1]] = self
            self.coords = new_coords
            self.moved = True

    def follow_vector(source, vector, board, color):
        for scalar in range(1, 8):
            move = (s + scalar * d for (s, d) in zip(source, vector))
            if (not all(0 <= c < 8 for c in move)) or\
                    (board[move[0]][move[1]] and board[move[0]][move[1]].black == color):
                break
            yield move
            if board[move[0]][move[1]]:
                break

    class Rook(Piece):
        def __str__(self):
            return emotes.rook[self.black]
        def moves(self, board):
            yield from follow_vector(self.coord, (0, 1), board)
            yield from follow_vector(self.coord, (0, -1), board)
            yield from follow_vector(self.coord, (1, 0), board)
            yield from follow_vector(self.coord, (-1, 0), board)

    class Bishop(Piece):
        def __str__(self):
            return emotes.bishop[self.black]
        def moves(self, board):
            yield from follow_vector(self.coord, (1, 1), board)
            yield from follow_vector(self.coord, (1, -1), board)
            yield from follow_vector(self.coord, (-1, 1), board)
            yield from follow_vector(self.coord, (-1, -1), board)

    class Queen(Piece):
        def __str__(self):
            return emotes.queen[self.black]
        def moves(self, board):
            yield from Rook.moves(board)
            yield from Bishop.moves(board)

    class King(Piece):
        def __str__(self):
            return emotes.king[self.black]
        def moves(self, board):
            for i in [-1, 0, 1]:
                for j in [-1, 0, 1]:
                    if i == j and j == 0:
                        continue
                    if 0 <= self.coords[0] + i < 8 and 0 <= self.coords[1] + j < 8:
                        yield (self.coords[0] + i, self.coords[1] + j)
            if not self.moved and\
                    self.board[self.coords[0]][7] and\
                    not self.board[self.coords[0]][7].moved and\
                    self.board[self.coord[0]][5] and\
                    self.board[self.coord[0]][6]:
                yield (self.coord[0], 6)
        def move(self, board, new_coord):
            old = self.coord
            Piece.move(self, board, new_coord)
            if new_coord == (old[0], 6) and old[1] - new_coord[1] == 2:
                board[old[0]][5], board[old[0]][7] = board[old[0]][7], Space(old[0], 7)
                board[old[0]][5].coords = (old[0], 5)
                board[old[0]][5].moved = True

    class Pawn(Piece):
        def __str__(self):
            return emotes.pawn[self.black]
        def moves(self, board):
            direction = 1 if self.black else -1
            if 0 <= self.coords[0] + direction < 8:
                for i in [-1, 0, 1]:
                    if i == 0 or board[self.coords[0] + direction][self.coords[1] + i]:
                        yield (self.coords[0] + direction, self.coords[1] + i)
        def move(self, board, new_coord):
            pass

    class Knight(Piece):
        def __str__(self):
            return emotes.knight[self.black]
        def moves(self, board):
            directions = [(2, 1), (1, 2), (-1, 2), (-1, -2), (1, -2), (-2, 1), (-2, -1), (2, -1)]
            moves = map(lambda direction: tuple(c + d for (c, d) in zip(self.coords, direction)), directions)
            yield from filter(lambda m: all(0 <= i < 8 for i in m), moves)
