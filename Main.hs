{- CPSC 411 -- Assignment 4 -- M+- Lexer and Parser, using BNFC
 - Author: Clayton Vis
 - Date:  2018
 -
 - Note: Keith Van Der Mullen and myself worked closely on this assignment, thus 
 - the structure of our submissions may be similar. Groupwork of up to 2 was
 - specified to be allowed in Dr. Cockett's assignment specifications, namely 
 - the document located at "http://pages.cpsc.ucalgary.ca/~robin/class/411/M+/M+.txt".
 -}


-- automatically generated by BNF Converter
module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import LexM
import ParM
import SkelM
import PrintM
import AbsM
import AST
import SemIR
import GenIR


import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          let astTree = tree
                          putStrLn $ show astTree
                          putStrLn "Test over"
                          exitSuccess


showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  conts <- readFile (args !! 0)
  let toks = myLexer conts
  let parse = pProg toks
  case parse of
    Ok tree -> do
      putStrLn "---------\n-- AST --\n---------"
      let astTr = transProg tree
      putStrLn $ show astTr
      putStrLn "\n\n--------\n-- IR --\n--------"
      let ir = convProg astTr
      putStrLn $ show ir
    Bad msg -> putStrLn msg


testC = "\nvar n:int;\nfun fib(n:int):int\n   {var z:int;\n    begin\n      if n =< 1 then z:= 1\n      else z:= fib(n-1) + fib(n-2);\n      return z;\n    end};\nbegin\n  read n;\n  print fib(n);\nend\n"

tokC = myLexer testC
parC = pProg tokC
astC = transProg $ (\(Ok t) -> t) parC

simpT = "var x[2]:int;\n fun exp(b:int):int\n { var z:int;\n begin if b=0 then z:= 1\n else z:= x[1] * exp(b-1);\n return z;\n end};\n begin\n read x[0];\n read x[1];\n print exp(x[0]);\n end"

tokT = myLexer simpT
parT = pProg tokT
astT = transProg $ (\(Ok t) -> t) parT
