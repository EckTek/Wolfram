module Parser where

-- Haskell
import Text.Read
-- from hs file
import Utils


checkOptValues :: Int -> [String] -> IO ()
checkOptValues 0 [] = return ()
checkOptValues n (x:xs) = case x of
       "null"  -> exit "Error: Invalid option."
       "error" | n == 5 -> exit "Error: Invalid --rule option value."
               | n == 4 -> exit "Error: Invalid --start option value."
               | n == 3 -> exit "Error: Invalid --lines option value."
               | n == 2 -> exit "Error: Invalid --window option value."
               | n == 1 -> exit "Error: Invalid --move option value."
       _       -> checkOptValues (n - 1) (xs)


findRuleOpt :: [String] -> IO ()
findRuleOpt (x:xs) | notElem "--rule" (x:xs) = exit "Error: No --rule option found."
                   | otherwise = return ()


isPositiveInt :: [Char] -> Bool
isPositiveInt [] = True
isPositiveInt (x:xs) | (x >= '0' && x <= '9') = isPositiveInt xs
                     | otherwise = False

replaceNth :: [a] -> (Int, a) -> [a]
replaceNth [] _ = []
replaceNth (_:xs) (0, a) = a:xs
replaceNth (x:xs) (n, a) | n < 0 = x:xs
                            | otherwise = x:replaceNth xs (n - 1, a)


getOptions :: [String] -> [String] -> [String]
getOptions (x:val:xs) opt = case x of
       "--rule"   -> case isPositiveInt val of
              True -> getOptions xs (replaceNth opt (0, val))
              _ -> getOptions xs (replaceNth opt (0, "error"))
       "--start"  -> case isPositiveInt val of
              True -> getOptions xs (replaceNth opt (1, val))
              _ -> getOptions xs (replaceNth opt (1, "error"))
       "--lines"  -> case isPositiveInt val of
              True -> getOptions xs (replaceNth opt (2, val))
              _ -> getOptions (val:xs) (replaceNth opt (2, "-1"))
       "--window" -> case readMaybe val :: Maybe Int of
              Just x -> getOptions xs (replaceNth opt (3, val))
              _ -> getOptions xs (replaceNth opt (3, "error"))
       "--move"   -> case readMaybe val :: Maybe Int of
              Just x -> getOptions xs (replaceNth opt (4, val))
              Nothing -> getOptions xs (replaceNth opt (4, "error"))
       otherwise -> ["null"]
getOptions  _ opt = opt
