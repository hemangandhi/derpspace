#include <iostream>
#include <list>
#include <unordered_set>
#include <utility>
#include <variant>

#include "traverse.hpp"

using graph_traversal::GraphTraversal;

template <typename V>
class OncePerVertexVisitor {
   public:
    std::pair<bool, std::monostate> visit(const V& vertex) {
        return std::make_pair(visited_.insert(vertex).second, std::monostate());
    }

   private:
    std::unordered_set<V> visited_;
};

template <typename V, bool IsDfs>
class ListFringe {
   public:
    V pop() {
        if constexpr (IsDfs) {
            V val = fringe_.back();
            fringe_.pop_back();
            return val;
        } else {
            V val = fringe_.front();
            fringe_.pop_front();
            return val;
        }
    }

    V& peek() { return fringe_.front(); }

    void push(const V& v, std::monostate w) { fringe_.push_back(v); }

    bool empty() { return fringe_.empty(); }

   private:
    std::list<V> fringe_;
};

std::vector<int> collatz(const int& x) {
    std::vector<int> v;
    v.push_back((x % 2 == 0) ? x / 2 : 3 * x + 1);
    return v;
}

int main(int argc, char** argv) {
    std::function<std::vector<int>(const int&)> cp = collatz;
    OncePerVertexVisitor<int> dfs_visits;
    ListFringe<int, true> dfs_fringe;
    dfs_fringe.push(2, std::monostate());
    for (std::optional<int> v :
         GraphTraversal<int, std::monostate, ListFringe<int, true>,
                        OncePerVertexVisitor<int>>(&dfs_fringe, &dfs_visits,
                                                   &cp)) {
        std::cout << "DFS visiting : " << v.value_or(-8) << std::endl;
    }
    OncePerVertexVisitor<int> bfs_visits;
    ListFringe<int, false> bfs_fringe;
    bfs_fringe.push(2, std::monostate());
    for (std::optional<int> v :
         GraphTraversal<int, std::monostate, ListFringe<int, false>,
                        OncePerVertexVisitor<int>>(&bfs_fringe, &bfs_visits,
                                                   &cp)) {
        std::cout << "BFS visiting : " << v.value_or(-8) << std::endl;
    }
    return 0;
}
