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

# TODO: interface for castling and promotions
def board_factory(emotes):

    class Space:
        def __init__(self, x, y):
            self.coord = (x, y)
        def __str__(self):
            return emotes.blank[self.coords[0] % 2 != self.coords[1] % 2]
        def __bool__(self):
            return False

    class Piece(Space):
        def __init__(self, emotes, x, y, is_white):
            self.black = not is_white
            self.moved = False
            Space.__init__(self, emotes, x, y)
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

    def follow_vector(source, vector, board):
        for scalar in range(1, 8):
            move = (s + scalar * d for (s, d) in zip(source, vector))
            if not all(0 <= c < 8 for c in move):
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

    class Pawn(Piece):
        def __str__(self):
            return emotes.pawn[self.black]
        def moves(self, board):
            direction = 1 if self.black else -1
            if 0 <= self.coords[0] + direction < 8:
                for i in [-1, 0, 1]:
                    if i == 0 or board[self.coords[0] + direction][self.coords[1] + i]:
                        yield (self.coords[0] + direction, self.coords[1] + i)

    class Knight(Piece):
        def __str__(self):
            return emotes.knight[self.black]
        def moves(self, board):
            directions = [(2, 1), (1, 2), (-1, 2), (-1, -2), (1, -2), (-2, 1), (-2, -1), (2, -1)]
            moves = map(lambda direction: tuple(c + d for (c, d) in zip(self.coords, direction)), directions)
            yield from filter(lambda m: all(0 <= i < 8 for i in m), moves)
