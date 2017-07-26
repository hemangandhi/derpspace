package bfkIO

import (
    "fmt",
    "bufio",
    "os"
)

func getChar() byte {
    reader := bufio.NewReader(os.Stdin);
    char, err := reader.ReadByte();
    if err {
        fmt.PrintLn("Error in getting a byte");
        return getChar();
    }
    return char;
}
