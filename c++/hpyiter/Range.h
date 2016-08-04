#include <iterator>

namespace hPyIter{

class Range : public std::iterator<std::bidirectional_iterator_tag, int> {
public:
	Range(int stop);
	Range(int start, int stop);
	Range(int start, int stop, int step);
        Range(const Range& cp);
	Range& operator++();
	Range& operator++(int j);
	Range& operator--();
	Range& operator--(int j);
	bool operator==(Range& other);
	bool operator!=(Range& other);
	int operator*();
	int operator->();
	int operator[](int ind);
	bool hasNext();
	bool hasPrev();
	int size() const;
	Range& begin() const;
	Range& end() const;
private:
	int start, stop, step, curr;
};

}
