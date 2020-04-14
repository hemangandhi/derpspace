#include "board.h"
#include <algorithm>
#include <utility>

namespace {

  void GrowShape(int x, int y, int to_match, const Board& board, std::vector<std::pair<int, int>>* wavefront) {
    if (board[x + 1, y] == to_match) {
      wavefront->emplace_back({x + 1, y});
    }
    if (board[x - 1, y] == to_match) {
      wavefront->emplace_back({x - 1, y});
    }
    if (board[x, y + 1] == to_match) {
      wavefront->emplace_back({x, y + 1});
    }
    if (board[x, y - 1] == to_match) {
      wavefront->emplace_back({x, y - 1});
    }
  }
  
  int SizeOfShape(int x, int y, const Board& board, std::vector<std::vector<bool>>* seen_points){
    // if the bounds checking or "initialization" checking fails, we can just ignore the cell
    if (board[x, y] == 0) return 0;
    
    // This is a DFS since we pop and emplace_back.
    std::vector<std::pair<int, int>> wavefront{{x, y}};
    int size = 0;
    while (!wavefront.empty()) {
      std::pair<int, int> current_point = wavefront.pop_back();
      if (0 <= current_point.first && current_point.first < seen_points->size()) {
        if (0 <= current_point.second && current_point.second < (*seen_points)[current_point.first].size()) {
          seen_points[current_point.first][current_point.second] = true;
	  size++;
	  GrowShape(current_point.first, current_point.second, board, &wavefront);
	}
      }
    }
    return size;
  }
  
}

namespace filomino {

  Board::Board(int width, int height) :
    width_(width),
    height_(height),
    board_(std::vector<std::vector<int>>(height,
					 std::vector<int>(width, 0))) {}

  int Board::operator[](int x, int y) {
    if (0 <= y && y < height_) {
      if (0 <= x && x < width_) {
        return board_[y][x];
      }
    }
    return 0;
  }

  std::optional<int&> Board::operator[](int x, int y) {
    if (0 <= y && y < height_) {
      if (0 <= x && x < width_){
	return board_[y][x];
      }
    }
    return std::optional();
  }

  bool Board::Validate() {
    std::vector<std::vector<bool>> seen_board(height_, std::vector<bool>(width_, false));
    for (int i = 0; i < height_; i++) {
      for (int j = 0; j < width_; j++) {
        if (seen_board[i][j]) continue;

        if (SizeOfShape(i, j, *this, &seen_board) != (*this)[i, j]) {
          return false;
	}
      }
    }
    return true;
  }

}
