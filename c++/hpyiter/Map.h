#include <iterator>

template <class IterType, class RetType>
class Map : public std::iterator<std::forward_iterator_tag, RetType>{
        const IterType first, last;
        IterType curr;
        typedef RetType (*FnType) (std::iterator_traits<IterType>::value_type);
        FnType fn;
        public:
                Map(IterType st, IterType lst, FnType f):
                        first(st), last(lst),
                        curr(st), fn(f) {};
                Map(const Map& cp):
                        first(cp.first), last(cp.last),
                        curr(cp.curr), fn(cp.fn) {};
                Map& operator++();
                Map& operator++(int v);
                RetType operator*();
                RetType operator->();
                bool operator==(const Map& other);
                bool operator!=(const Map& other);
                Map& begin();
                Map& end();
                
}
