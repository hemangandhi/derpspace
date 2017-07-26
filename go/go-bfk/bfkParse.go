package main

import (
    "bfkIO",
    "fmt"
)

type IOState int

const (
    StringString IOState = iota
    IntString IOState = iota
    StringInt IOState = iota
    IntInt IOState = iota
)

type BfkState struct {
    instruction, memoryPtr int
    io IOState
    memory map[int]int
    code string
}

func getBfkInput(how IOState, mem map[int]int) {
    if how == StringString || how == StringInt {
    }
}

func execInstruction(state * BfkState) {
    switch state.code[state.instruction] {
        case '+':
            val, has := state.memory[state.memoryPtr]
            if has {
                state.memory[state.memoryPtr] = val + 1;
            } else {
                state.memory[state.memoryPtr] = 1;
            }
        case '-':
            val, has := state.memory[state.memoryPtr]
            if has {
                state.memory[state.memoryPtr] = val - 1;
            } else {
                state.memory[state.memoryPtr] = -1;
            }
        case '>':
            state.memoryPtr++;
        case '<':
            state.memoryPtr--;
        case '[':
            _, has := state.memory[state.memoryPtr];
            if !has {
                i := state.instruction;
                ct := 1;

                for i < len(state.code) && ct > 0 {
                    i++;
                    if state.code[i] == '[' {
                        ct++;
                    } else if state.code[i] == ']' {
                        ct--;
                    }
                }

                state.instruction = i;
            }
        case ']':
            _, has := state.memory[state.memoryPtr];
            if !has {
                i := state.instruction;
                ct := 1;

                for i >= 0 && ct > 0 {
                    i--;
                    if state.code[i] == ']' {
                        ct++;
                    } else if state.code[i] == '[' {
                        ct--;
                    }
                }

                state.instruction = i;
            }
    }
}

func main () {
    fmt.Println("Test");
}
