module Chatterbot where
import Utilities
import System.Random
import Data.Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
-- pointfree function
stateOfMind brain = do
   r <- randomIO :: IO Float
   let phrasepairs = map (\i->(fst i, pick r (snd i))) brain
   return $ rulesApply phrasepairs

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply  =  (Maybe.fromMaybe [] . ) . transformationsApply "*" reflect

reflect :: Phrase -> Phrase
reflect [] = []
reflect (s:phrases)
    | elem s (map fst reflections) = x:reflect phrases
    | otherwise = s:reflect phrases
    where
      x :: String
      x = (Maybe.fromMaybe "" (lookup s reflections))

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")
-- prepare = rwords . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map ( map2 (words. map toLower, map words))

--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply [] phrase = phrase
reductionsApply (r:rs) phrase
    | match '*' (unwords (fst r)) (unwords phrase) == Nothing =  reductionsApply rs phrase
    | otherwise = reductionsApply reductions (substitute "*" (snd r) (Maybe.fromMaybe [] (match "*" (fst r) phrase)))

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]

substitute a (x:xs) bs
    | bs == [] = xs
    | x == a && xs == [] = bs
    | xs == [] = [x]
    | x == a = bs ++ (substitute a xs bs)
    | otherwise = x:(substitute a xs bs)


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match wildcard (x:xs) (y:ys)
    | x == wildcard = orElse (singleWildcardMatch (x:xs) (y:ys)) (longerWildcardMatch (x:xs) (y:ys))
    | x == y = match wildcard xs ys
    | otherwise = Nothing

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:xs) (y:ys) = match wc xs ys >> Just [y]
longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) $ match wc (wc:ps) xs

-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions

-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wildcard func xs p
    | (match wildcard (fst p) xs) == Nothing = Nothing
    | otherwise = Just (substitute wildcard (snd p) m)
    where
      m = (func r)
        where
          r = (Maybe.fromMaybe [] (match wildcard (fst p) xs))


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wildcard func (p:ps) xs
    | transformationApply wildcard func xs p == Nothing = transformationsApply wildcard func ps xs
    | otherwise =  transformationApply wildcard func xs p
