module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
-- skip is to avoid compile error, completely pointless otherwise
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip |
    Begin [Statement] |
    While  Expr.T Statement |
    Read String |
    Write Expr.T |
    Comment String
    deriving Show

-- choose any of these parsers
parser = assignmentParser ! ifParser ! skipParser ! beginParser ! whileParser ! readParser ! writeParser ! commentParser

-- parser for assignment
-- word -> (x, :=3) -> skickar :=3 till accept -> (:=, 3), -# vaskar resultatet från accept dvs. := och tar resultatet från första dvs. x
-- då är bara den resterande strängen kvar -> 3 -> (x, 3)
-- removes the ;
assignmentParser = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

-- if m - m/k*k then\ skip;\ else\ write m;\
-- m - m/k*k , then\ skip;\ else\ write m;\
-- m - m/k*k , skip;\ else\ write m;\
-- m - m/k*k, (skip, else\ write m;\)
-- m - m/k*k, (skip, write m;\)
--  m - m/k*k, (skip, write m)
-- accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
ifParser = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((expr, stmnt1), stmnt2) = If expr stmnt1 stmnt2

skipParser = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip

-- man vill köra parse för varje stmt till och med end
beginParser = accept "begin" -# iter (parse #- spaces) #- require "end" >-> buildBegin
buildBegin = Begin

-- 'while' expr 'do' statement
whileParser = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (expr, stmt) = While expr stmt

-- \read n;\
readParser = accept "read" -# word #- require ";" >-> buildRead
buildRead = Read

-- write r;\
writeParser = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite = Write

-- commentParser should get -- and then strings until \n
commentParser = accept "--" -# readLine #- require "\n" >-> buildComment
buildComment = Comment

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
-- assign a variable to an expression in dictionary
exec (Assignment variable expr : stmts) dict input = exec stmts (Dictionary.insert (variable, Expr.value expr dict) dict) input
-- If expr thenStmts elseStmts = Statement
-- (If expr thenStmts elseStmts : stmts) = Statement:stmts
exec (If expr thenStmts elseStmts : stmts) dict input =
    if Expr.value expr dict > 0 -- cond
    then exec (thenStmts : stmts) dict input
    else exec (elseStmts : stmts) dict input
-- skip statement
exec (Skip : stmts) dict input = exec stmts dict input
-- do a number of statemenets from a list
exec (Begin beginStmns : stmts) dict input = exec (beginStmns ++ stmts) dict input
-- in then we want to keep looping, therefore add stmnt we want to run again
-- the stmnt might not be a while statement so to assure we return into the While loop we add our own While statement with the same expr and statement
-- this way the expr is being updated
exec (While expr stmnt : stmts) dict input =
    if Expr.value expr dict > 0 -- cond
    then exec (stmnt: While expr stmnt : stmts) dict input
    else exec stmts dict input
-- read a variable and add it to dictionary with the first integer from input
exec (Read variable : stmts) dict input = exec stmts (Dictionary.insert (variable, head input) dict) input
-- add the result from Expr.value to the final result from the rest of the exec's -> [Integer]
exec (Write expr : stmts) dict input = Expr.value expr dict : exec stmts dict input
-- comment, just skip over lines; stmnts
exec (Comment s : stmts) dict input = exec stmts dict input

-- add indents
indentSize = 2
indents n = replicate (n * indentSize ) ' '

-- indents n ++ "begin\n" ++ concatMap (shw (n + 1)) bStmts ++ indents n ++ "end\n"
shw ::Int -> Statement -> String
shw n (Assignment variable expr) = indents n ++ variable ++ ":=" ++ Expr.toString expr ++ ";\n"
shw n (If expr thenStmts elseStmts) = indents n ++ "if " ++ Expr.toString expr ++ " then\n" ++ shw (n+1) thenStmts ++ indents n ++ "else\n" ++ shw (n+1) elseStmts
shw n Skip = indents n ++ "skip" ++ ";\n"
-- give concatMap (show (n+1)) to apply on all stmts in beginStmns
shw n (Begin beginStmns) = indents n ++ "begin\n" ++ concatMap (shw (n+1)) beginStmns ++ indents n ++ "end\n"
shw n (While expr stmnt) = indents n ++ "while " ++ Expr.toString expr ++ "do\n" ++ shw (n+1) stmnt
shw n (Read variable) = indents n ++ "read " ++ variable ++ ";\n"
shw n (Write expr) = indents n ++ "write " ++ Expr.toString expr ++ ";\n"
shw n (Comment s) = indents n ++ "-- " ++ s ++ ";\n"

instance Parse Statement where
  parse = parser
  -- start with 0 indent
  toString = shw 0
