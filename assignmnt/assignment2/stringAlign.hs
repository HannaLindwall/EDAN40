import Helpers
import qualified Data.List as List

type AlignmentType = (String,String)

outputOptAlignments string1 string2 = putStrLn ("There are " ++ show (length xs) ++ " optimal alignments:\n\n" ++ (buildString xs) ++ "There are " ++ show (length xs) ++ " optimal alignments!")
    where xs = optAlignments string1 string2

buildString [] = []
buildString (x:xs) = ((fst x) ++ "\n" ++ (snd x) ++ "\n\n") ++ (buildString xs)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([], [])]
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs [])
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments [] ys)
optAlignments (s1:string1) (s2:string2) = maximaBy currScore (concat [attachHeads s1 s2 (optAlignments string1 string2)
    , attachHeads '-' s2 (optAlignments (s1:string1) string2)
    , attachHeads s1 '-' (optAlignments string1 (s2:string2)) ])

-- func är funktionen som sorterar på högsta scoren, tar in ett par
currScore :: (String, String) -> Int
currScore pair = simScore (fst pair) (snd pair)

simScore :: String -> String -> Int
simScore [] [] = 0
simScore _ [] = (-1)
simScore [] _ = (-1)
simScore (s1:string1) (s2:string2)
    | s1 == '-' || s2 == '-' = scoreSpace + (simScore string1 string2)
    | s1 /= s2 = scoreMisMatch + (simScore string1 string2)
    | otherwise = scoreMatch + (simScore string1 string2)
    where
      scoreMatch = 0
      scoreMisMatch = (-1)
      scoreSpace = (-1)

similarityScore' :: String -> String -> Int
similarityScore' [] [] = 0
similarityScore' _ [] = (-1)
similarityScore' [] _ = (-1)
similarityScore' (s1:string1) (s2:string2) =
   max (similarityScore string1 string2 + score' (s1,s2))
   (max (similarityScore string1 (s2:string2) + score' (s1,'-')) (similarityScore (s1:string1) string2 + score' ('-',s2)))

score' (x, '-') = (-1)
score' ('-', y) = (-1)
score' (x,y) = if x == y then 0 else (-1)

similarityScore :: Eq a => [a] -> [a] -> Int
similarityScore xs ys = simLen (length xs) (length ys)
  where
    simLen i j = simTable!!i!!j
    simTable = [[ simEntry i j | j<-[0..]] | i<-[0..] ]

    simEntry :: Int -> Int -> Int
    simEntry _ 0 = (-1)
    simEntry 0 _ = (-1)
    simEntry i j = max (simLen (i-1) (j-1) + score(x,y)) (max (simLen i (j-1) + score(x,y) (simLen (i-1) j + score(x,y)))
      where
         x = xs!!(i-1)
         y = ys!!(j-1)

 score (x, '-') = (-1)
 score ('-', y) = (-1)
 score (x,y) = if x == y then 0 else (-1)

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
