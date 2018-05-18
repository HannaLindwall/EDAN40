module Parser(module CoreParser, T, digit, digitVal, chars, letter, err, lit, number, iter, accept, require, token, readLine, spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #-


type T a = Parser a
-- Parser a :: String -> Maybe (a, String)
err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

-- take a parser and iterate over something based on the type of the parser
iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []

cons(a, b) = a:b

-- do something and then return snd parser
(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >-> snd

-- do something and then return fst parser
(#-) :: Parser a -> Parser b -> Parser a
m #- n = m # n >-> fst

-- take the isSpace parser and iterate over the string based on it
spaces :: Parser String
spaces = iter (char ? isSpace)

token :: Parser a -> Parser a
token m = m #- spaces

-- take the string and apply char on it, then ? gets that it should apply isAlpha on the first item returned by char
letter :: Parser Char
letter = char ? isAlpha

-- remove spaces and check if word
word :: Parser String
word = token (letter # iter letter >-> cons)

-- iterate m i = m # iterate m (i-1) >-> cons
-- chars return a parser therefore we don't need to add a parser after calling chars
chars :: Int -> Parser String
chars 0 = return []
chars n = char # chars (n-1) >-> cons

accept :: String -> Parser String
accept w = token (chars (length w)) ? (==w)

-- uses accept to see if phrase can be accepted based on word
require :: String -> Parser String
require w  = accept w ! err ("Program error: expecting " ++ w)

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char
digit = char ? isDigit

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

-- iterate over chars until \n is found
readLine :: Parser String
readLine = iter (char ? (/='\n'))
