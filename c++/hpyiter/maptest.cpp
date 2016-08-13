#include "Map.h"
#include "Range.h"
#include <iostream>

using namespace hPyIter;

template class Map<Range, int>;

int main(int argc, char ** argv){
        Map<Range, int> v {Range(9), Range(9).end(), 
                [](int x){
                        return x * x;
                }};
        Map<Range, int> v2 = v.end();
        for(; v != v2; v++){
                std::cout << *v << std::endl;
        }
        delete &v2;
        return 0;
}
