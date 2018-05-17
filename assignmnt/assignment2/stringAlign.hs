import Helpers
import qualified Data.List as List
import qualified Data.Function as Function

type AlignmentType = (String,String)

scoreSpace = -1
scoreMisMatch = -1
scoreMatch = 0
-- 2.e)
outputOptAlignments string1 string2 = putStrLn ("There are " ++ show (length xs) ++ " optimal alignments:\n\n" ++ buildString xs ++ "There are " ++ show (length xs) ++ " optimal alignments!")
    where xs = optAlignments string1 string2

buildString [] = []
buildString xs = foldr (\ x -> (++) (fst x ++ "\n" ++ snd x ++ "\n\n")) [] xs

-- 2.d)
optAlignments' :: String -> String -> [AlignmentType]
optAlignments' [] [] = [([],[])]
optAlignments' (x:xs) [] = attachHeads x '-' (optAlignments' xs [])
optAlignments' [] (y:ys) = attachHeads '-' y (optAlignments' [] ys)
optAlignments' (s1:string1) (s2:string2) = maximaBy currScore (concat [attachHeads s1 s2 (optAlignments' string1 string2)
    , attachHeads '-' s2 (optAlignments' (s1:string1) string2)
    , attachHeads s1 '-' (optAlignments' string1 (s2:string2)) ])

-- 3
optAlignments :: String -> String -> [AlignmentType]
optAlignments xs ys = maximaBy currScore (snd (alignLen (length xs) (length ys)))
  where
    alignLen i j = alignTable!!i!!j
    alignTable = [[ alignEntry i j | j<-[0..]] | i<-[0..] ]

    alignEntry :: Int -> Int -> (Int, [AlignmentType])
    alignEntry 0 0 = (0, [([],[])])
    alignEntry i 0 = (i * scoreSpace, [(take i xs, replicate i '-')])
    alignEntry 0 j = (j * scoreSpace, [(replicate j '-', take j ys)])
    alignEntry i j = (head (map fst findAligns), concatMap snd findAligns)
      where
        findAligns = maximaBy fst [(scr1 , algns1), ( scr2 , algns2), ( scr3, algns3)]
        scr1 = fst (alignLen (i-1) (j-1)) + score (x,y)
        scr2 = fst (alignLen i (j-1)) + score (x,'-')
        scr3 = fst (alignLen (i-1) j) + score ('-',y)
        algns1 = attachTails x y (snd (alignLen (i-1) (j-1)))
        algns2 = attachTails '-' y (snd (alignLen i (j-1)))
        algns3 = attachTails x '-' (snd (alignLen (i-1) j))
        x = xs!!(i-1)
        y = ys!!(j-1)

--2.a)
similarityScore' :: String -> String -> Int
similarityScore' [] [] = scoreMatch
similarityScore' _ [] = scoreSpace
similarityScore' [] _ = scoreSpace
similarityScore' (s1:string1) (s2:string2) =
   max (similarityScore string1 string2 + score (s1,s2))
   (max (similarityScore string1 (s2:string2) + score (s1,'-')) (similarityScore (s1:string1) string2 + score ('-',s2)))

similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore _ [] = scoreSpace
similarityScore [] _ = scoreSpace
similarityScore xs ys = simLen (length xs) (length ys)
  where
    simLen i j = simTable!!i!!j
    simTable = [[ simEntry i j | j<-[0..]] | i<-[0..] ]

    simEntry :: Int -> Int -> Int
    simEntry _ 0 = scoreSpace
    simEntry 0 _ = scoreSpace
    simEntry i j = max (simLen (i-1) (j-1) + score (x,y)) (max (simLen i (j-1) + score(x,'-')) (simLen (i-1) j + score('-',y)))
      where
        x = xs!!(i-1)
        y = ys!!(j-1)

score (x, '-') = scoreSpace
score ('-', y) = scoreSpace
score (x,y) = if x == y then scoreMatch else scoreMisMatch

-- 2.b)
-- h1 and h2 gets attached as heads to the lists inside the every tuple in aList
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails h1 h2 aList = [(xs ++ [h1], ys ++ [h2]) | (xs,ys) <- aList]

-- currScore är funktionen som sorterar på högsta scoren, tar in ett par
currScore :: (String, String) -> Int
currScore (a,b) = simScore a b

-- 2.c)
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = getListElements valueFcn (reverse (sort valueFcn xs))

sort func = List.sortBy (compare `Function.on` func)

getListElements _ [] = []
getListElements _ [a] = [a]
getListElements valueFcn (x:xs)
    | valueFcn x == valueFcn (head xs) = x:getListElements valueFcn xs
    | otherwise = [x]

simScore :: String -> String -> Int
simScore [] [] = scoreMatch
simScore _ [] = scoreSpace
simScore [] _ = scoreSpace
simScore (s1:string1) (s2:string2)
    | s1 == '-' || s2 == '-' = scoreSpace + simScore string1 string2
    | s1 /= s2 = scoreMisMatch + simScore string1 string2
    | otherwise = scoreMatch + simScore string1 string2
