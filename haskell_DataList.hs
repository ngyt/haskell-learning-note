module List2
(intersperse2,
	intercalate2,
	concat2,
	concatMap2,
	and2,
	or2,
	any2,
	all2,
	iterate2,
	splitAt2) where

import Data.List
import Data.Foldable

intersperse2 :: a -> [a] -> [a]
intersperse2 _ [] = []
intersperse2 _ (x:[]) = [x]
intersperse2 m (x:xs) = foldl (\acc x -> acc ++ [m] ++ [x]) [x] xs

intercalate2 :: [a] -> [[a]] -> [a]
intercalate2 m (x:xs) = foldl (\acc x -> acc ++ m ++ x) x xs

--transpose2 :: [[a]] ->[[a]]
--transpose2 [] = []
--transpose2 xs = [foldl (\acc x -> acc ++ x) [] (map (take 1) xs)] ++ transpose2 (map (drop 1) xs)

concat2 :: [[a]] -> [a]
concat2 xs = foldl (\acc x -> acc ++ x) [] xs

concatMap2 :: (a -> [b]) -> [a] -> [b]
concatMap2 f xs = foldl (\acc x -> acc ++ f x ) [] xs

and2 :: [Bool] -> Bool
and2 xs = foldl (\acc x -> if x then acc else False) True xs

or2 :: [Bool] -> Bool
or2 xs = foldl (\acc x -> if x then True else acc) False xs

any2 :: (a -> Bool) -> [a] -> Bool
any2 f xs = foldl (\acc x -> if (f x) then True else acc) False xs

all2 :: (a -> Bool) -> [a] -> Bool
all2 f xs = foldl (\acc x -> if (f x) then acc else False) True xs

iterate2 :: (a -> a) -> a -> [a]
iterate2 f xs = [xs] ++ iterate2 f (f xs)

splitAt2 :: Int -> [a] -> ([a], [a])
splitAt2 n xs = (take n xs, drop n xs)

-- takeWhile
-- dropWhile
-- span
-- sort group

