#include <array>
#include <tuple>
#include <unordered_set>
#include <unordered_map>
#include <iostream>

enum class CellState {circle, cross, empty};

typedef std::tuple<int,int> Point;
typedef std::tuple<Point, Point> Move;
typedef std::unordered_set<Move> MoveSet;
typedef std::Array<std::Array<CellState, 3>, 3> InnerBoard;
typedef std::Array<std::Array<InnerBoard, 3>, 3> Board_t;
typedef std::unordered_map<Point, CellState> InnerBdStates;
//Board_t is a 4-D array as used in super TTT

class GameState{
        MoveSet moves;
        Board_t board;
        InnerBdStates won;
        CellState turn;
        public:
                GameState();
                void move(Move m);
                GameState virtualMove(Move m);
                friend std::ostream& operator<<(std::ostream& out, const GameState& game);
                const Board_t getBoard();
                CellState getTurn();
                CellState getVictor();
        private:
                void printInnerBd(std::ostream& out, const InnerBoard inner, const Point innerPt);
}
