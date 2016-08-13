#include "Map.h"

using namespace hPyIter;

template <class I, class R>
Map<I, R>& Map<I, R>::operator++(int j){
        curr++;
        return *this;
}

template <class I, class R>
Map<I, R>& Map<I, R>::operator++() const{
        Map& ret = new Map(*this);
        ret++;
        return ret;
}

template <class I, class R>
R Map<I, R>::operator*() const{
        return fn(*curr);
}

template <class I, class R>
R Map<I, R>::operator->() const{
        return **this;
}

template <class I, class R>
bool Map<I, R>::operator==(const Map<I, R>& other) const{
        bool v = other.first == this->first;
        v = v && other.curr == this->curr;
        v = v && other.last == this->last;
        v = v && other.fn == this->fn;
        return v;
}

template <class I, class R>
bool Map<I, R>::operator!=(const Map<I, R>& other) const{
        return !(*this == other);
}

template <class I, class R>
Map<I, R>& Map<I, R>::begin() const{
        Map<I, R>& ret = new Map(*this);
        ret.curr = first;
        return ret;
}

template <class I, class R>
Map<I, R>& Map<I, R>::end() const{
        Map<I, R>& ret = new Map(*this);
        ret.curr = last;
        return ret;
}
