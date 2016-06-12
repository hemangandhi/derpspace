-- Count words http://book.realworldhaskell.org/read/getting-started.html exercise 3

main = interact wordCount
        where wordCount input = show (length (words input)) ++ "\n"
