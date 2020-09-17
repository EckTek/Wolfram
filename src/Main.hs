--
-- EPITECH PROJECT, 2020
-- FUN_pushswap_checker_2019
-- File description:
-- pushswap_checker
--

-- Haskell import
import System.Environment
import Text.Read
import System.Exit

-- from hs file
import Utils
import Parser
import Wolfram
import Rules

main :: IO ()
main = do
    args <- getArgs
    case args of
       []     -> exit "Error: No arguments. Rerun with -h or --help."
       (x:[]) -> exit "Error: No argument value. Rerun with -h or --help."
       (x:xs) | (x == "-h" && null xs) -> help helpMsg
              | (x == "--help" && null xs) -> help helpMsg
       _-> findRuleOpt args
    let strOpts = getOptions args defaultOptions
    checkOptValues 5 strOpts
    let intOpts = map (read::String -> Int) strOpts
    let firstLine = findFirstLine (intOpts !! 0) (intOpts !! 1) "  *  "
    wolfram intOpts (head intOpts) (intOpts !! 2) firstLine
    exitWith (ExitSuccess)

findFirstLine :: Int -> Int -> String -> String
findFirstLine _ 0 line = line
findFirstLine r startValue line = findFirstLine r (startValue - 1) (applyRules r line)