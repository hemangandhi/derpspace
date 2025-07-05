package main

import "fmt"

type VoidPtr interface{}

type Stream struct {
	value VoidPtr
	next  func() Stream
}

func streamMap(fn func(...VoidPtr) VoidPtr, str ...Stream) {
	getNexts := func(ss ...Stream) []Stream {
		sts := []Stream()
		for _, s := range ss {
			append(sts, s.next())
		}
		return sts
	}

	getValues := func(ss ...Stream) []VoidPtr {
		sts := []Stream()
		for _, s := range ss {
			append(sts, s.value)
		}
		return sts
	}

	nextStream := func() Stream {
		val := fn(getValues(str)...)
		composition = func() Stream {
			return streamMap(fn, getNexts(str)...)
		}

		return Stream{val, composition}
	}

	nextStream()
}

func main() {
	ones = Stream{1, func() {
		return ones
	}}
}
