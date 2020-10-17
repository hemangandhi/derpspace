#include <coroutine>
#include <cstddef> // size_t
#include <cmath> // pow

// A quick concept for what we think containers are.
template<typename C, typename T>
concept bool Container = requires(C a, std::size_t b) {
  typename C<T>; // We need to have the container of T,
  { a.size(); } -> std::size_t; // with a size,
  { a[b]; } -> const T&; // and element access.
};

template<typename T, Container<T> Cont>
std::generator<T> get_subset_from(const Cont<T>& cont, unsigned int flags) {
  for (std::size_t i = 0; i < cont.size(); i++) {
    if ((flags >> i) & 1) {
      co_yield cont[i];
    }
  }
}

template<typename T, Container<T> Cont>
std::generator<std::generator<T>> subsets_iter(const Cont<T>& cont) {
  std::size_t subsets {Math.pow(2.0, cont.size())};
  for(std::size_t i = 0; i < subsets; i++) {
    co_yield get_subset_from(cont, i);
  }
}

int main(){
    
}
