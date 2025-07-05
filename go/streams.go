package main

import "fmt"

type VoidPtr interface{}

type Stream struct {
	value VoidPtr
	next  func() Stream
}

func streamMap(fn func(...VoidPtr) VoidPtr, str ...Stream) Stream {
	getNexts := func(ss []Stream) []Stream {
		sts := []Stream{}
		for _, s := range ss {
			sts = append(sts, s.next())
		}
		return sts
	}

	getValues := func(ss []Stream) []VoidPtr {
		sts := []VoidPtr{}
		for _, s := range ss {
			sts = append(sts, s.value)
		}
		return sts
	}

	nextStream := func() Stream {
		val := fn(getValues(str)...)
		composition := func() Stream {
			return streamMap(fn, getNexts(str)...)
		}

		return Stream{val, composition}
	}

	return nextStream()
}

func main() {
	var ones Stream
	ones = Stream{1, func() Stream {
		return ones
	}}
}
