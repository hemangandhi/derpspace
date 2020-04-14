#include <vector>
#include <optional>

namespace filomino {

  class Board{
  public:
    Board(int width, int height);
    int operator[](int x, int y) const;
    int& operator[](int x, int y);
    bool Validate() const;
    bool TrySolve();
  private:
    std::vector<std::vector<int>> board_;
  };

}
