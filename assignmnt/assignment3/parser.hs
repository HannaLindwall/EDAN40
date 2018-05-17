import qualified Data.Maybe as Maybe
import qualified Data.Char as Char

data Expr = Num Int | Var String | Add Expr Expr| Sub Expr Expr | Mul Expr Expr | Div Expr Expr


semicolon :: String -> Maybe (Char, String)
semicolon [] = Nothing
semicolon (s:str1)
    | s == ';' = Just(';', buildString Char.isSpace str1)
    | otherwise = Nothing

buildString func (s:str1)
    | func s = buildString func str1
    | otherwise = s : str1

buildBecomeString func [] = []
buildBecomeString func (s:str1)
    | not (func s) = buildBecomeString func str1
    | otherwise = s : str1

buildBecomeNonString func [] = []
buildBecomeNonString func (s:str1)
    | not (func s) = s : buildBecomeNonString func str1
    | otherwise = []

becomes [] = Nothing
becomes str1 = Just(buildBecomeNonString Char.isAlphaNum str1, buildBecomeString Char.isAlpha str1)

char :: Parser Char
char (c:cs) = Just(c,cs)
char [] = Nothing
