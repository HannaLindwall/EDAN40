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
    Write Expr.T
    deriving Show

parser = assignmentParser ! ifParser ! skipParser ! beginParser ! whileParser ! readParser ! writeParser

-- parser for assignment
assignmentParser = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

ifParser
buildIf

skipParser
buildSkip

beginParser
buildBegin

whileParser
buildWhile

readParser
buildRead

writeParser
buildWrite

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


instance Parse Statement where
  parse = error "Statement.parse not implemented"
  toString = error "Statement.toString not implemented"
