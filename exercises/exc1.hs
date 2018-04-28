import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Function as Function

maxi :: (Ord a) => a -> a -> a
maxi x y
    | x < y       = y
    | otherwise   = x


-- sumsq n returns 1*1 + 2*2 + ... + n*n
-- Use recursion.

sumsquares 0 = 0
sumsquares n = n*n + sumsquares(n - 1)

-- Now use mapping instead
sumsquares' n = sum $ map(\i->i*i) n

-- hanoi
hanoi 0 = 0
hanoi 1 = 1
hanoi n = 2^(n-1) + hanoi(n-1)

-- smallestfactor, nextFactor
nextFactor k n
    | n `mod` k /= 0  = nextFactor (k+1) n
    | otherwise       = k

smallestFactor n = nextFactor 2 n

--data
type Year = Int
type Month = Int
type Day = Int
data Date  = Date Year Month Day

days m
    | m == 4 || m == 6 || m == 9 || m == 11 = 30
    | otherwise = 31
-- var 4e år
daysInMonth m y
    | y `mod` 4 /= 0 && m == 2 = 28
    | y `mod` 4 == 0 && m == 2 = 29
    | otherwise = days m

validDate :: Date -> Bool
validDate (Date y m d)
    | y < 0 = False
    | m < 1 || m > 12 = False
    | d < 1 || d > daysInMonth m y = False
    | otherwise = True
--data D (a,b,c) = (a,b,c)
--data Date = Year Int | Month Int | Day Int
--multiply
multiply :: Num a => [a] -> a
multiply [] = 1
multiply (x:xs) = x * (multiply xs)

--substitute
substitute :: Eq a => a -> a -> [a] -> [a]
substitute a b xs = [if x == a then b else x | x <- xs]

--duplicates
duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates (x:xs) = x `elem` xs || duplicates xs
--removeDuplicates, starting from x and then adds the next element != x
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) =  x : (removeDuplicates [e | e <- xs])

--triads
pythagoras a b = a*a + b*b

triads n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a <= b, b <= c, pythagoras a b == c*c]

--isPermutation

removeX x ys = List.delete x ys

isPermutation :: Eq a => [a] -> [a] -> Bool

isPermutation (x:xs) ys
    | xs == [] && (removeX x ys) == [] = True
    | xs /= [] && (removeX x ys) == [] = False
    | xs == [] && (removeX x ys) /= [] = False
    | otherwise = isPermutation xs ( removeX x ys )

--isShortestAndLongest
sort xs = List.sortBy (\x y -> length x `compare` length y) xs

isShortestAndLongest xs = (head (sort xs), last (sort xs))

--mystery
-- returns the same list?
mystery xs = foldr (++) [] (map (\y -> [y]) xs)

-- getIndex for element in list
getIndex index xs ys
    | length xs == 0 = length ys
    | xs == ((drop index . take (index + (length xs))) ys ) = index
    | otherwise = getIndex (index + 1) xs ys

--hej
