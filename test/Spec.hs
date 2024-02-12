module Spec (runTests) where

import Control.Applicative(empty)
import Parser

runTests :: IO ()
runTests = do
  results <-
    sequence
      [ test_charP1,
        test_charP2,
        test_trim,
        test_commentP1,
        test_commentP2,
        test_idP,
        test_typeSpecP,
        test_valueP,
        test_assignmentStmtP
      ]
  print $
    if and results
      then "tests passed successfully!"
      else "some tests failed:-- " ++ show results

test_charP1 :: IO Bool
test_charP1 = do
  putStrLn ""

  let result = (runParser $ Parser.charP 'h') "hello"
      eq = result == return ("ello", 'h')
  print result
  print eq
  return eq

test_charP2 :: IO Bool
test_charP2 = do
  putStrLn ""

  let result = (runParser $ Parser.charP 'h') "goodbye"
      eq = result == empty
  print result
  print eq
  return eq

test_trim :: IO Bool
test_trim = do
  putStrLn ""

  let trimResult = Parser.preprocess untrimmedSource
      eq = trimResult == trimmedSource
  print trimResult
  print eq
  return eq
  where
    untrimmedSource =
      "\
      \    pure function func3(a,b,c)  !a pure function can have no side-effects.\n\
      \        implicit none\n\
      \\
      \        integer, intent(in) :: a,b,c\n\
      \        integer :: func3\n\
      \        func3 = a*b*c\n\
      \    end function func3\n\
      \"
    trimmedSource =
      "\
      \PURE FUNCTION FUNC3(A,B,C)  !A PURE FUNCTION CAN HAVE NO SIDE-EFFECTS.\n\
      \IMPLICIT NONE\n\
      \INTEGER, INTENT(IN) :: A,B,C\n\
      \INTEGER :: FUNC3\n\
      \\
      \FUNC3 = A*B*C\n\
      \END FUNCTION FUNC3\n\
      \"

test_commentP1 :: IO Bool
test_commentP1 = do
  putStrLn ""

  let result = runParser Parser.commentP "! this is a comment\n"
      eq = result == return ("", ())
  print result
  print eq
  return eq

test_commentP2 :: IO Bool
test_commentP2 = do
  putStrLn ""

  let result = runParser Parser.commentP "this is NOT! a comment\n"
      eq = result == empty
  print result
  print eq
  return eq

test_idP :: IO Bool
test_idP = do
  putStrLn ""

  let results = map (runParser idP) [
        "ACN93874",
        "AEIRT",
        "B4 58VM372",
        "1A"  
        ]
      eq = results == [
          return ("", "ACN93874"),
          return ("", "AEIRT"),
          return (" 58VM372", "B4"),
          empty
        ]
  print results
  print eq
  return eq

test_typeSpecP :: IO Bool
test_typeSpecP = do
  putStrLn ""

  let results = map (runParser typeSpecP) [
        "INTEGER",
        "LOGICAL",
        "CHARACTER",
        "IINT"  
        ]
  print results
  return $ match' results
    where 
      match' [ 
        Just ("", FInteger _),
        Just ("", FLogical _),
        Just ("", FCharacter _),
        Nothing
        ] = True
      match' _ = False

test_valueP :: IO Bool
test_valueP = do
  putStrLn ""

  let results = map (runParser valueP) [
        "30927049",
        "043975",
        ".TRUE.",  
        ".FALSE.",  
        ".BBB."  
        ]
  print results
  return $ match' results
    where 
      match' [ 
        Just ("", FInteger 30927049),
        Nothing,
        Just ("", FLogical True),
        Just ("", FLogical False),
        Nothing
        ] = True
      match' _ = False


test_assignmentStmtP :: IO Bool
test_assignmentStmtP = do
  putStrLn ""

  let results = map (runParser assignmentStmtP) [
        "NICER_INTEG3R_WITH_VALUE = 31",
        "INT_W_B4D_LITERAL = 03",
        "VARIABLE_WITH_WS             =            3",
        "LOGICAL_VAR_ = .TRUE.",
        "FALSE_ = .FALSE.",
        "_BAD_HANDLE_ = .FALSE."
        ]
  print results
  return $ match'' results
    where 
      match'' = (== [ 
        Just ("", (:=) "NICER_INTEG3R_WITH_VALUE" (valueToExpr $ FInteger 31)),
        Nothing,
        Just ("", (:=) "VARIABLE_WITH_WS" (valueToExpr $ FInteger 3)),
        Just ("", (:=) "LOGICAL_VAR_" (valueToExpr $ FLogical True)),
        Just ("", (:=) "FALSE_" (valueToExpr $ FLogical False)),
        Nothing
        ])
