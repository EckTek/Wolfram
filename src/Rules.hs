module Rules where

import Utils

applyRules :: Int -> [Char] -> [String] -> [Char]
applyRules r line rulesets | (r >= 0 && r <= 255) = allRules line "  " (rulesets !! r)
                           | otherwise = "error"

allRules :: [Char] -> [Char] -> [Char] -> [Char]
allRules (l:m:r:xs) line ruleset
   | (l == '*' && m == '*' && r == '*') = allRules (m:r:xs) (line ++ [ruleset !! 0]) ruleset
   | (l == '*' && m == '*' && r == ' ') = allRules (m:r:xs) (line ++ [ruleset !! 1]) ruleset
   | (l == '*' && m == ' ' && r == '*') = allRules (m:r:xs) (line ++ [ruleset !! 2]) ruleset
   | (l == '*' && m == ' ' && r == ' ') = allRules (m:r:xs) (line ++ [ruleset !! 3]) ruleset
   | (l == ' ' && m == '*' && r == '*') = allRules (m:r:xs) (line ++ [ruleset !! 4]) ruleset
   | (l == ' ' && m == '*' && r == ' ') = allRules (m:r:xs) (line ++ [ruleset !! 5]) ruleset
   | (l == ' ' && m == ' ' && r == '*') = allRules (m:r:xs) (line ++ [ruleset !! 6]) ruleset
   | (l == ' ' && m == ' ' && r == ' ') = allRules (m:r:xs) (line ++ [ruleset !! 7]) ruleset
allRules _ line ruleset = line ++ "  "
