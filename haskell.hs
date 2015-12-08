-- import module
import Data.List
-- import particular functions
import Data.List (nub, sort)
-- import module except some functions
import Data.List hiding (nub)
-- qualified imports. Need to do `Data.Map.filter` instead of `filter`
import qualified Data.Map
-- or as following. This will provide a simpler way of using, `M.filter`
import qualified Data.Map as M


-- patterns
tell :: (Show a) => [a] -> String
tell [] = "empty list"
tell (x:[]) = "one element"
tell (x:y:[]) =  "two elements"
tell (x:y:_) = "three elements and above"

-- patterns
length2 :: (Num b) => [a] -> b
length2 [] = 0
length2 (_:xs) = 1 + length2 xs

-- guard
bmiTeller :: (RealFloat a) => a -> String
bmiTeller bmi
    | bmi <= 18.5 = "Eat more."
    | bmi <= 25.0 = "OK."
    | bmi <= 30.0 = "Lose some weight."
    | otherwise   = "BIG"

-- guard with function
bmiTeller2 :: (RealFloat a) => a -> a -> String
bmiTeller2 weight height
    | bmi <= 18.5 = "Eat more."
    | bmi <= 25.0 = "OK."
    | bmi <= 30.0 = "Lose some weight."
    | otherwise   = "BIG"
    where bmi = weight / height ^ 2

-- binding
-- [ x | x <- [50..100], x `mod` 7 == 3]
calsBmis :: (RealFloat a) => [(a, a)] -> [a]
calsBmis xs = [bmi | (w, h) <- xs, let bmi = w/h^2, bmi > 25.0]

-- case expressions
head2 :: [a] -> a
head2 xs = case xs of [] -> error "No Head of Empty lists."
                      (x:_) -> x

-- case expressions
describeList :: [a] -> String
describeList xs = "This is " ++ case xs of [] -> "an empty list."
                                           [x] -> "one element list."
                                           xs -> "a long list."

-- 
take2 :: (Num i, Ord i) => i -> [a] -> [a]
take2 n _
    | n <= 0    = []
take2 _ [] 		= []
take2 n (x:xs) = x : take2 (n-1) xs

-- Quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort[a | a <- xs, a <= x]
        biggerSorted = quicksort[a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

-- Higher order function
zipWith2 :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith2 _ _ [] = []
zipWith2 _ [] _ = []
zipWith2 f (x:xs) (y:ys) = f x y : zipWith2 f xs ys

-- Collatz sequences
collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
    | even n = n: collatz (n `div` 2)
    | odd n  = n: collatz(n*3 + 1)

-- fold
sum2 :: (Num a) => [a] -> a
sum2 xs = foldl (\m n -> m + n) 0 xs

elem2 :: (Eq a) => a -> [a] -> Bool
elem2 y ys = foldl (\m n -> if n == y then True else m ) False ys

map2 :: (a -> b) -> [a] -> [b]
map2 f xs = foldr (\x acc -> f x : acc) [] xs

-- practice
max2 :: (Ord a) => [a] -> a
max2 xs = foldl1 (\x y -> if x > y then x else y) xs

reverse2 :: [a] -> [a]
reverse2 xs = foldl (\acc x -> x : acc ) [] xs

product2 :: (Num a) => [a] -> a
product2 xs = foldl (\acc x -> acc * x) 1 xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f xs = foldr (\x acc -> if f x then x : acc else acc) [] xs

-- head2
-- last2
sqrtSums :: Int  
sqrtSums = length ( takeWhile (<1000) (scanl1 (+) (map sqrt [1..]) ) ) + 1  


oddSquareSum :: Integer  
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
