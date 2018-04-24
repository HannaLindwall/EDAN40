import qualified Data.List as List
import qualified Data.Maybe as Maybe

map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs

substitute :: Eq a => a -> [a] -> [a] -> [a]

substitute a (x:xs) bs
    | bs == [] = xs
    | xs == [] = [x]
    | x == a = bs ++ (substitute a xs bs)
    | otherwise = x:(substitute a xs bs)


match :: Eq a => a -> [a] -> [a] -> Maybe [a]

match _ [] [] = Just []
match _ [] _ = Just []
match _ _ [] = Just []
match wildcard (x:xs) (y:ys)
    | x /= wildcard && x == y = match wildcard xs ys
    | x == wildcard && xs == ys = singleWildcardMatch (x:xs) (y:ys)
    | x == wildcard && List.isInfixOf xs ys = longerWildcardMatch (x:xs) (y:ys)
    | x == wildcard && elem wildcard xs = longerWildcardMatch (x: (drop 0 . take (getIndex 0 [wildcard] xs)) xs) (y:ys)
    | otherwise = Nothing

singleWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:xs) (y:ys) = match wc xs ys >> Just [y]

longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
longerWildcardMatch (x:xs) (y:ys) = Just (y:(drop 0 . take (getIndex 0 xs ys)) ys)

getIndex index xs ys
    | xs == ((drop index . take (index + (length xs))) ys ) = index
    | otherwise = getIndex (index + 1) xs ys
