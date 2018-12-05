#include <iterator> //just for the std tags
#include <vector>
#include <functional>
#include <optional>

template <class T>
class Subset{
    public:
        template <class T2>
        class SubsetIter: public std::iterator<std::random_access_iterator_tag, std::vector<T>>{
            public:
                SubsetIter(std::iterator<std::random_access_iterator_tag, T>& source,
                        int loc, int length): set(source), loc(loc), len(length){}
                SubsetIter<T2>& operator+= (int n){
                    loc += n;
                    return this;
                }
                SubsetIter<T2>& operator-= (int n){
                    loc -= n;
                    return this;
                }
                SubsetIter<T2> operator+ (int n){
                    return SubsetIter(this->set, this->loc + n, this->len);
                }
                SubsetIter<T2> operator++ (){
                    this += 1;
                    return SubsetIter(this->set, this->n - 1, this->len);
                }
                SubsetIter<T2> operator-- (){
                    this -= 1;
                    return SubsetIter(this->set, this->n + 1, this->len);
                }
                SubsetIter<T2> operator++ (int n){
                    this += 1;
                    return this;
                }
                SubsetIter<T2> operator-- (int n){
                    this -= 1;
                    return this;
                }
                SubsetIter<T2> operator- (int n){
                    return SubsetIter(this->set, this->loc - n, this->len);
                }
                std::vector<T2> operator[] (int n){
                    return *SubsetIter(this->set, this->loc + n, this->len);
                }
                std::vector<T2> operator* (){
                    std::vector<T2> test;
                    for(int i = 0; i < this->len; i++){
                        if(loc & (0x1 << i)){
                            test.push_back(set[i]);
                        }
                    }
                    return test;
                }
                bool operator<= (SubsetIter<T2>& other){
                    return this->loc <= other->loc;
                }
                bool operator>= (SubsetIter<T2>& other){
                    return this->loc >= other->loc;
                }
                bool operator< (SubsetIter<T2>& other){
                    return this->loc < other->loc;
                }
                bool operator> (SubsetIter<T2>& other){
                    return this->loc > other->loc;
                }
                bool operator== (SubsetIter<T2>& other){
                    return this->loc == other->loc;
                }
            private:
                std::iterator<std::random_access_iterator_tag, T>& set;
                int loc;
                int len;
        };
        Subset(std::iterator<std::random_access_iterator_tag, T>& source, int length):set(source), len(length){}
        SubsetIter<T> begin(){
            return SubsetIter<T>(this->set, 0, this->len);
        }
        SubsetIter<T> end(){
            int n = 1;
            for(int i = 0; i < this->len; i++) n *= 2;
            return SubsetIter<T>(this->set, n, this->len);
        }
    private:
        std::iterator<std::random_access_iterator_tag, T>& set;
        int len;

};

template <class T>
struct Myterator{
    T datum;
    std::function<std::optional<Myterator<T>>()> next;
};


template <class T>
Myterator<T> concat(Myterator<T> left, Myterator<T> right){
    
}

int main(){
    
}
