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

	vals := []T{}
	for _, s := range str {
		vals = append(vals, s.value)
	}

	val := fn(vals...)
	composition := func() *Stream[T] {
		nexts := getNexts(str)
		if nexts == nil {
			return nil
		}
		return streamMap(fn, *nexts...)
	}

	return &Stream[T]{val, composition}

}

func streamConcat[T any](strs ...*Stream[T]) *Stream[T] {
	if len(strs) == 0 {
		return nil
	}
	if strs[0] == nil {
		return streamConcat(strs[1:]...)
	}
	return &Stream[T]{
		strs[0].value,
		func() *Stream[T] {
			// TODO: be sure that the below copies.
			strs[0] = strs[0].next()
			return streamConcat(strs...)
		},
	}
}

func main() {
	var ones Stream[int]
	ones = Stream[int]{1, func() *Stream[int] {
		return &ones
	}}
}
