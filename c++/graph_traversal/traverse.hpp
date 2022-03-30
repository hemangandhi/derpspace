#ifndef __DERPSPACE_CPP_GRAPH_TRAVERSAL_TRAVERSAL_H_
#define __DERPSPACE_CPP_GRAPH_TRAVERSAL_TRAVERSAL_H_

#include <concepts>
#include <functional>
#include <iterator>

namespace graph_traversal {

// An interface for a fringe: a set of known neighbors that have yet to
// be visited. An instance, C, of this interface would contain vertices of type
// V that are ranked with weight W.
template <typename C, typename V, typename W>
concept fringe_interface = requires(C& c, const V& v, const W& w) {
    // Remove a node to visit it.
    { c.pop() } -> std::convertible_to<V>;
    // Find the next node that would be visited (but don't count it as visited).
    { c.peek() } -> std::convertible_to<V&>;
    // Add a node to the fringe.
    {c.push(v, w)};
    // Check if anything is in the heap.
    { c.empty() } -> std::convertible_to<bool>;
};

// An interface for visiting the nodes in the graph. A node can be visited
// any number of times.  An instance, C, of this interface would visit vertices
// of type V and weigh them with W.
template <typename C, typename V, typename W>
concept visitor_interface = requires(C& c, const V& v) {
    // Visit a node, returning its "weight" and whether it needs to be
    // added to the fringe.
    { c.visit(v) } -> std::convertible_to<std::pair<bool, W>>;
};

// Traverses a graph using the fring and visitor provided.
// The traversal does not own the fringe or visitor (so either may be reused).
// The traversal is, abstractly, a container, but it merely provides iterators
// and a way for these iterators to interact with one another.
template <typename V, typename W, fringe_interface<V, W> Fringe,
          visitor_interface<V, W> Visitor>
class GraphTraversal {
   public:
    // A function to get the neighbors of a vertex in the graph.
    using NeighborFn = std::function<std::vector<V>(const V&)>;

    GraphTraversal(Fringe* fringe, Visitor* visitor, NeighborFn* neighbor_fn)
        : fringe_(fringe),
          visitor_(visitor),
          neighbor_fn_(neighbor_fn),
          deepest_iter_(0) {}

    // An iterator over the traversal. This actually does the "traversing."
    // For use by STL algorithms, this class implements the entire interface
    // for the std::forward_iterator_tag (so it's copyable and assignable).
    //
    // A key difference, however, is that if you make a copy and then advance
    // one of the iterators, the other one is considered "invalid" and will only
    // provide std::nullopt. This is done to avoid copying vertices.
    class Iterator {
       public:
        friend class GraphTraversal;
        using iterator_category = std::forward_iterator_tag;
        using different_type = std::ptrdiff_t;
        // Copys the vertex from the graph.
        using value_type = std::optional<V>;
        // Provides pointer access into the graph.
        using pointer = std::optional<V*>;
        // Allows for mutation of the graph node.
        using reference = std::optional<std::reference_wrapper<V>>;

        // If you have some `Iterator it`, this is `*it`.
        reference operator*() {
            if (parent_->deepest_iter_ > depth_) {
                return std::nullopt;
            }
            return parent_->fringe_->peek();
        }

        // If you have some `Iterator it`, this is the first `->` in
        // `it->value_or(&some_v)->thing()`.
        pointer operator->() {
            if (parent_->deepest_iter_ > depth_) {
                return std::nullopt;
            }
            return &parent_->fringe_->peek();
        }

        // If you have some `Iterator it`, this is `++it`.
        Iterator operator++() {
            // If we're at the end, do nothing.
            if (end_sentinel_) return *this;
            // Else, we've gone deeper.
            depth_++;
            // If we're not yet the "furthest" iterator,
            // stop.
            if (depth_ <= parent_->deepest_iter_) {
                return *this;
            }
            // If we're the furthest iterator and the
            // fringe is empty, also stop.
            // Here, we don't update the graph's "deepest"
            // iterator so that other iterators can reach
            // this state quicker (ie. with fewer calls to ++).
            if (parent_->fringe_->empty()) {
                end_sentinel_ = true;
                return *this;
            }
            // Otherwise, we're the deepest and we have more to
            // explore, so let's visit what the fringe tells us to.
            parent_->deepest_iter_++;
            V vertex = parent_->fringe_->pop();
            for (const V& neighbor : (*parent_->neighbor_fn_)(vertex)) {
                if (const auto [add_to_fringe, weight] =
                        parent_->visitor_->visit(neighbor);
                    add_to_fringe) {
                    parent_->fringe_->push(neighbor, weight);
                }
            }
            return *this;
        }

        // If you have some `Iterator it`, this is `it++`.
        Iterator operator++(int) {
            // Note on copying: this is cheap since the iterator is merely
            // an int, a pointer, and a bool. If we stored the fringe and
            // visitor in the iterators, we may have to copy them and their
            // internal data structures.
            Iterator tmp = *this;
            ++(*this);
            return tmp;
        }

        // Two iterators are equal when they're from the same graph, and
        // are at the same depth or both at the end.
        friend bool operator==(const Iterator& l, const Iterator& r) {
            return l.parent_ == r.parent_ &&
                   (l.depth_ == r.depth_ ||
                    (r.end_sentinel_ && l.end_sentinel_));
        }

       private:
        // This makes any non-end iterator.
        Iterator(int depth, GraphTraversal* parent)
            : depth_(depth), parent_(parent), end_sentinel_(false) {}
        // This makes any iterator.
        Iterator(int depth, GraphTraversal* parent, bool is_end)
            : depth_(depth), parent_(parent), end_sentinel_(is_end) {}

        int depth_;
        GraphTraversal* parent_;
        // We need an "end_sentinel_" because C++ assumes we have some "end"
        // that we can talk about in (amortized) O(1) time, but we can't do
        // that without entirely traversing the graph or imposing this
        // arbitrary sentinel.
        bool end_sentinel_;
    };

    Iterator begin() { return Iterator(0, this); }
    Iterator end() { return Iterator(-1, this, true); }

   private:
    Fringe* fringe_;
    Visitor* visitor_;
    NeighborFn* neighbor_fn_;
    int deepest_iter_;
};

}  // namespace graph_traversal

#endif  // __DERPSPACE_CPP_GRAPH_TRAVERSAL_TRAVERSAL_H_
