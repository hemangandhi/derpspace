#include <stdexcept>
#include "stdafx.h"
#include "Range.h"

Range::Range(int start, int stop, int step) {
	if ((stop < start && step > 0) || step == 0 || (start < stop && step < 0))
		throw std::logic_error("Cannot make infinite range!");
	this->start = start;
	this->step = step;
	this->stop = stop;
	this->curr = 0;
}

Range::Range(int start, int stop) {
	if (stop < start)
		throw std::logic_error("Cannot make infinite range!");
	this->start = start;
	this->stop = stop;
	this->step = 1;
	this->curr = 0;
}

Range::Range(int stop) {
	if (start < 0)
		throw std::logic_error("Cannot make infinite range!");
	this->start = 0;
	this->stop = stop;
	this->step = 1;
	this->curr = 0;
}

int Range::size() {
	int s = stop - start;
	if (s % step == 0)
		return s / step;
	else
		return s / step + 1;
}

bool Range::hasNext() {
	return curr < size();
}

bool Range::hasPrev() {
	return curr >= 0;
}

int Range::operator[](int ind) {
	if (ind >= size() || ind < 0)
		throw std::out_of_range("Index out of bounds!");
	return start + ind*step;
}

Range& Range::operator++() {
	if (!hasNext())
		throw std::out_of_range("Passed the limit!");
	curr++;
	return *this;
}

Range& Range::operator++(int j) {
	if (!hasNext())
		throw std::out_of_range("Passed the limit!");
	Range* other = new Range(start, stop, step);
	other->curr = curr;
	curr++;
	return *other;
}

Range& Range::operator--() {
	if (!hasNext())
		throw std::out_of_range("Passed the limit!");
	curr--;
	return *this;
}

Range& Range::operator--(int j) {
	if (!hasPrev())
		throw std::out_of_range("Passed the limit!");
	Range* other = new Range(start, stop, step);
	other->curr = curr;
	curr--;
	return *other;
}

bool Range::operator==(Range& other) {
	return other.start == start && other.stop == stop && other.step == step && other.curr == curr;
}

bool Range::operator!=(Range& other) {
	return !(*this == other);
}

int Range::operator*() {
	return *this[curr];
}

int Range::operator->() {
	return **this;
}

Range& Range::begin() {
	return Range(start, stop, step);
}

Range& Range::end() {
	Range& r = Range(start, stop, step);
	r.curr = size();
	return r;
}
