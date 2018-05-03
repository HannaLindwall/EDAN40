import Helpers
import qualified Data.List as List

type AlignmentType = (String,String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments a [] = [(a,[])]
optAlignments [] a = [([],a)]
optAlignments (s1:string1) (s2:string2) = maximaBy similarityScore [((s1:string1),(s2:string2)), (s1:('-':string1), (s2:string2)), ((s1:string1), s2:('-':string2))]

similarityScore :: (String, String) -> Int
similarityScore ([], []) = 0
similarityScore (_, []) = 0
similarityScore ([], _) = 0
similarityScore ((s1:string1), (s2:string2))
    | s1 == '-' || s2 == '-' = scoreSpace + (similarityScore (string1, string2))
    | s1 /= s2 = scoreMisMatch + (similarityScore (string1, string2))
    | otherwise = scoreMatch + (similarityScore (string1, string2))
    where
      scoreMatch = 0
      scoreMisMatch = (-1)
      scoreSpace = (-1)
-- h1 and h2 gets attached as heads to the lists inside the every tuple in aList
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

sort func xs = List.sortBy (\x y -> func x `compare` func y) xs

getListElements _ [] = []
getListElements _ [a] = [a]
getListElements valueFcn (x:xs)
    | valueFcn x == valueFcn (head xs) = x:(getListElements valueFcn xs)
    | otherwise = [x]


maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = reverse (getListElements valueFcn (reverse (sort valueFcn xs)))
