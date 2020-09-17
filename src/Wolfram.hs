module Wolfram where

import Rules

printLines :: [Char] -> Int -> Int -> IO ()
printLines line window move = do
        let offset = window `div` 2 - (length line) `div` 2 + move
        let spaces = resizeLine line offset
        putStrLn (addSpaces spaces window)

resizeLine :: [Char] -> Int -> [Char]
resizeLine line offset
    | offset < 0 = resizeLine (tail line)   (offset + 1)
    | offset > 0 = resizeLine (" " ++ line) (offset - 1)
    | otherwise = init (init line)

addSpaces :: [Char] -> Int -> [Char]
addSpaces line win | (length line < win) = addSpaces (line ++ [' ']) win
                   | (length line > win) = addSpaces (init line) win
                   | otherwise = line
    
    
wolfram :: [Int] -> Int -> Int -> [Char] -> IO ()
wolfram opt rule cnt line | (cnt == 0) = return ()
                          | (cnt < 0) = do
                                printLines (line) (opt !! 3) (opt !! 4)
                                wolfram opt rule (-1) (applyRules rule line)
                          | otherwise = do
                            printLines (line) (opt !! 3) (opt !! 4)
                            wolfram opt rule (cnt - 1) (applyRules rule line)
