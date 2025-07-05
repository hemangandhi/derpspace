package main

import "fmt"

type Stream[T any] struct {
	value T
	next  func() *Stream[T]
}

// Maps the fn over all the streams provided, returns a stream of the output.
// The returned stream ends when any input stream ends.
func streamMap[T any](fn func(...T) T, str ...Stream[T]) *Stream[T] {
	getNexts := func(ss []Stream[T]) *[]Stream[T] {
		sts := []Stream[T]{}
		for _, s := range ss {
			next := s.next()
			if next == nil {
				return nil
			}
			sts = append(sts, *next)
		}
		return &sts
	}

	getValues := func(ss []Stream[T]) []T {
		sts := []T{}
		for _, s := range ss {
			sts = append(sts, s.value)
		}
		return sts
	}

	val := fn(getValues(str)...)
	composition := func() *Stream[T] {
		nexts := getNexts(str)
		if nexts == nil {
			return nil
		}
		return streamMap(fn, *nexts...)
	}

	return &Stream[T]{val, composition}

}

func main() {
	var ones Stream[int]
	ones = Stream[int]{1, func() *Stream[int] {
		return &ones
	}}
}
