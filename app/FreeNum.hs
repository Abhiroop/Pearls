module FreeNum where

import Data.List
import Data.Array

minfree :: [Int] -> Int
minfree = search.checklist

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0,n) (zip (filter (<= n) xs) (repeat True))
                where n = length xs
