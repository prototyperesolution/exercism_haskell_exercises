module Bob (responseFor) where
import Data.Char


responseFor :: String -> String
responseFor sentence
    | not (any id (map (`elem` ['A'..'Z']) (map toUpper sentence))) = "Fine. Be that way!"
    | (all id (map (`elem` ['A'..'Z']) (init sentence))) =
        if (sentence !! ((length sentence)-1)) == '?' then "Calm down, I know what I'm doing!"
            else "Whoa, chill out!"
    | (sentence !! ((length sentence)-1)) == '?' = "Sure."
    | otherwise = "Whatever."