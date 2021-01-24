#include <coroutine>
#include <concepts>
#include <cstddef> // size_t
#include <cmath> // pow

template <std::input_iterator I>
generator<typename std::iterator_traits<I>::value_type>
get_subset_from(const I& cont, unsigned int flags) {
  for (std::size_t i = 0; i < cont.size(); i++) {
    if ((flags >> i) & 1) {
      co_yield cont[i];
    }
  }
}

template <std::input_iterator I>
generator<generator<typename std::iterator_traits<I>::value_type>>
subsets_iter(const I& cont) {
  std::size_t subsets {Math.pow(2.0, cont.size())};
  for(std::size_t i = 0; i < subsets; i++) {
    co_yield get_subset_from(cont, i);
  }
}

int main(){
    
}
