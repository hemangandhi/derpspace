#ifndef __DERPSPACE_CPP_GRAPH_TRAVERSAL_TRAVERSAL_H_
#define __DERPSPACE_CPP_GRAPH_TRAVERSAL_TRAVERSAL_H_

#include <concepts>
#include <functional>
#include <iterator>
#include <unordered_map>

namespace graph_traversal {

template <typename C, typename V, typename W>
concept fringe_interface = requires(C& c, const V& v, const W& w) {
    { c.pop() } -> std::convertible_to<V>;
    { c.peek() } -> std::convertible_to<V&>;
    {c.push(v, w)};
    { c.empty() } -> std::convertible_to<bool>;
};

template <typename C, typename V, typename W>
concept visitor_interface = requires(C& c, const V& v) {
    { c.visit(v) } -> std::convertible_to<std::pair<bool, W>>;
};

template <typename V, typename W, fringe_interface<V, W> Fringe,
          visitor_interface<V, W> Visitor>
class GraphTraversal {
   public:
    using NeighborFn = std::function<std::vector<V>(const V&)>;
    GraphTraversal(Fringe* fringe, Visitor* visitor, NeighborFn* neighbor_fn)
        : fringe_(fringe),
          visitor_(visitor),
          neighbor_fn_(neighbor_fn),
          deepest_iter_(0) {}

    class Iterator {
       public:
        friend class GraphTraversal;
        using iterator_category = std::forward_iterator_tag;
        using different_type = std::ptrdiff_t;
        using value_type = std::optional<V>;
        using pointer = std::optional<V*>;
        using reference = std::optional<std::reference_wrapper<V>>;

        reference operator*() {
            if (parent_->deepest_iter_ > depth_) {
                return std::nullopt;
            }
            return parent_->fringe_->peek();
        }

        pointer operator->() {
            if (parent_->deepest_iter_ > depth_) {
                return std::nullopt;
            }
            return &parent_->fringe_->peek();
        }

        Iterator operator++() {
            depth_++;
            if (depth_ <= parent_->deepest_iter_ || parent_->fringe_->empty()) {
                return *this;
            }
            parent_->deepest_iter_++;
            V vertex = parent_->fringe_->pop();
            for (const V& neighbor : (*parent_->neighbor_fn_)(vertex)) {
                if (const auto [add_to_fringe, weight] =
                        parent_->visitor_->visit(neighbor);
                    add_to_fringe) {
                    parent_->fringe_->push(neighbor, weight);
                }
            }
        }

        Iterator operator++(int) {
            Iterator tmp = *this;
            ++(*this);
            return tmp;
        }

        friend bool operator==(const Iterator& l, const Iterator& r) {
            return l.parent_ == r.parent_ &&
                   (l.depth_ == r.depth_ || l.end_sentinel_ == r.end_sentinel_);
        }

       private:
        Iterator(int depth, GraphTraversal* parent)
            : depth_(depth), parent_(parent), end_sentinel_(false) {}
        Iterator(int depth, GraphTraversal* parent, bool is_end)
            : depth_(depth), parent_(parent), end_sentinel_(is_end) {}

        int depth_;
        GraphTraversal* parent_;
        bool end_sentinel_;
    };

    Iterator begin() {
        return Iterator(0, this);
    }
    Iterator end() {
        return Iterator(0, this, true);
    }

   private:
    Fringe* fringe_;
    Visitor* visitor_;
    NeighborFn* neighbor_fn_;
    int deepest_iter_;
};

}  // namespace graph_traversal

#endif  // __DERPSPACE_CPP_GRAPH_TRAVERSAL_TRAVERSAL_H_
