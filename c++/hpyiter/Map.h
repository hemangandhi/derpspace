#include <iterator>

namespace hPyIter{

template <class I, class R>
class Map : public std::iterator<std::forward_iterator_tag, R>{
        const I first, last;
        I curr;
        typedef R (*FnType) (typename std::iterator_traits<I>::value_type);
        FnType fn;
        public:
                Map(I st, I lst, FnType f):
                        first(st), last(lst),
                        curr(st), fn(f) {};
                Map(const Map& cp):
                        first(cp.first), last(cp.last),
                        curr(cp.curr), fn(cp.fn) {};
                Map<I, R>& operator++() const;
                Map<I, R>& operator++(int v);
                R operator*() const;
                R operator->() const;
                bool operator==(const Map<I, R>& other) const;
                bool operator!=(const Map<I, R>& other) const;
                Map<I, R>& begin() const;
                Map<I, R>& end() const;
};

}
