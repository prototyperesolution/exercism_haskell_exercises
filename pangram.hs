module Pangram (isPangram) where

import Data.Char

isPangram :: String -> Bool
isPangram text = helper "abcdefghijklmnopqrstuvqxyz" (strip text) where
    strip :: String -> String
    strip = filter (\x -> x /= ' ')
    helper :: String -> String -> Bool
    helper [] _ = True
    helper _ [] = False 
    helper (a:as) (x:xs) = case (toUpper a) == (toUpper x) of
        True -> helper as (strip text)
        False -> helper (a:as) (xs)