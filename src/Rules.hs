module Rules where

applyRules :: Int -> [Char] -> [Char]
applyRules r line = case r of
    30 -> rule30 line "  "
    90 -> rule90 line "  "
    110 -> rule110 line "  "


rule30 :: [Char] -> [Char] -> [Char]
rule30 (l:m:r:xs) line = case (l, m, r) of
    ('*', '*', '*') -> rule30 (m:r:xs) (line ++ [' '])
    ('*', '*', ' ') -> rule30 (m:r:xs) (line ++ [' '])
    ('*', ' ', '*') -> rule30 (m:r:xs) (line ++ [' '])
    ('*', ' ', ' ') -> rule30 (m:r:xs) (line ++ ['*'])
    (' ', '*', '*') -> rule30 (m:r:xs) (line ++ ['*'])
    (' ', '*', ' ') -> rule30 (m:r:xs) (line ++ ['*'])
    (' ', ' ', '*') -> rule30 (m:r:xs) (line ++ ['*'])
    (' ', ' ', ' ') -> rule30 (m:r:xs) (line ++ [' '])
rule30 _ line = line ++ "  "


rule90 :: [Char] -> [Char] -> [Char]
rule90 (l:m:r:xs) line = case (l, m, r) of
    ('*', '*', '*') -> rule90 (m:r:xs) (line ++ [' '])
    ('*', '*', ' ') -> rule90 (m:r:xs) (line ++ ['*'])
    ('*', ' ', '*') -> rule90 (m:r:xs) (line ++ [' '])
    ('*', ' ', ' ') -> rule90 (m:r:xs) (line ++ ['*'])
    (' ', '*', '*') -> rule90 (m:r:xs) (line ++ ['*'])
    (' ', '*', ' ') -> rule90 (m:r:xs) (line ++ [' '])
    (' ', ' ', '*') -> rule90 (m:r:xs) (line ++ ['*'])
    (' ', ' ', ' ') -> rule90 (m:r:xs) (line ++ [' '])
rule90 _ line = line ++ "  "


rule110 :: [Char] -> [Char] -> [Char]
rule110 (l:m:r:xs) line = case (l, m, r) of
    ('*', '*', '*') -> rule110 (m:r:xs) (line ++ [' '])
    ('*', '*', ' ') -> rule110 (m:r:xs) (line ++ ['*'])
    ('*', ' ', '*') -> rule110 (m:r:xs) (line ++ ['*'])
    ('*', ' ', ' ') -> rule110 (m:r:xs) (line ++ [' '])
    (' ', '*', '*') -> rule110 (m:r:xs) (line ++ ['*'])
    (' ', '*', ' ') -> rule110 (m:r:xs) (line ++ ['*'])
    (' ', ' ', '*') -> rule110 (m:r:xs) (line ++ ['*'])
    (' ', ' ', ' ') -> rule110 (m:r:xs) (line ++ [' '])
rule110 _ line = line ++ "  "