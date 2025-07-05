package main

import "fmt"
import "iter"
import "math"

type Stream[T any] struct {
	value T
	next  func() *Stream[T]
}

// Maps the fn over all the streams provided, returns a stream of the output.
// The returned stream ends when any input stream ends.
func StreamMap[T any](fn func(...T) T, str ...Stream[T]) *Stream[T] {
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
		return StreamMap(fn, *nexts...)
	}

	return &Stream[T]{val, composition}

}

func StreamConcat[T any](strs ...*Stream[T]) *Stream[T] {
	if len(strs) == 0 {
		return nil
	}
	if strs[0] == nil {
		return StreamConcat(strs[1:]...)
	}
	return &Stream[T]{
		strs[0].value,
		func() *Stream[T] {
			new_streams := []*Stream[T]{strs[0].next()}
			for _, s := range strs[1:] {
				new_streams = append(new_streams, s)
			}
			return StreamConcat(new_streams...)
		},
	}
}

func streamOfSubsets[T any](set []T, offset int) Stream[[]T] {
	if offset >= len(set) {
		return Stream[[]T]{
			[]T{},
			func() *Stream[[]T] { return nil },
		}
	}
	next_sets := streamOfSubsets(set, offset+1)
	// NOTE: concat can't return nil for non-nil args.
	return *StreamConcat(&next_sets, StreamMap(func(strs ...[]T) []T {
		return append(strs[0], set[offset])
	}, next_sets))
}

func StreamOfSubsets[T any](set []T) Stream[[]T] {
	return streamOfSubsets(set, 0)
}

func StreamTakeN[T any](s *Stream[T], n int) *Stream[T] {
	if n == 0 || s == nil {
		return nil
	}
	return &Stream[T]{
		s.value,
		func() *Stream[T] {
			return StreamTakeN(s.next(), n-1)
		},
	}
}

func StreamGetArray[T any](s *Stream[T]) []T {
	a := []T{}
	for s != nil {
		a = append(a, s.value)
		s = s.next()
	}
	return a
}

func IterSubsets[T any](ts []T) iter.Seq[[]T] {
	return func(yield func([]T) bool) {
		setIndex := 0
		max := int(math.Pow(2.0, float64(len(ts))))
		for setIndex < max {
			s := []T{}
			for i, t := range ts {
				if setIndex&(1<<i) > 0 {
					s = append(s, t)
				}
			}
			if !yield(s) {
				return
			}
			setIndex += 1
		}
	}
}

func main() {
	var ones, nats Stream[int]
	ones = Stream[int]{1, func() *Stream[int] {
		return &ones
	}}
	nats = Stream[int]{1, func() *Stream[int] {
		return StreamMap(func(is ...int) int {
			return is[0] + is[1]
		}, ones, nats)
	}}
	first10 := StreamGetArray(StreamTakeN(&nats, 4))
	fmt.Printf("Nats: %q\n", first10)
	subsets := StreamOfSubsets(first10)
	fmt.Printf("Subsets: %q\n", StreamGetArray(&subsets))
	for s := range IterSubsets(first10) {
		fmt.Printf("Subset: %q\n", s)
	}
}
