module Main where

import Ast
import Parser ( parseProgram, parseReplStatement )
import System.Environment ( getArgs )
import Data.Map (Map, lookup, fromList, toList)
import Data.Maybe (fromMaybe, fromJust)
import Prelude hiding (lookup)
import System.Exit (exitFailure, exitWith, ExitCode (ExitFailure, ExitSuccess), exitSuccess)
import Interpreter
import Runtime
import Text.Parsec (ParseError)
import System.IO
import Errors

getFileName :: IO String
getFileName = do
  args <- getArgs
  case args of
    [fileName] -> return fileName
    _ -> do
      print "Invalid args count"
      exitWith (ExitFailure 1)

parse :: String -> IO MSProgram
parse text =
  case parseProgram text of
    Right x -> return x
    Left err -> do
      putStrLn "Parsing error:"
      print err
      exitWith (ExitFailure 1)

loadInitialData :: MSProgram -> IO MSRuntimeData
loadInitialData p = do
  let (res, msData) = run (loadContract p) emptyMSRuntime
  case res of
    Right () -> return msData
    Left err -> do
      print err
      exitWith (ExitFailure 1)

getLineWithOutput :: String -> IO String
getLineWithOutput s = do
    putStr s
    hFlush stdout
    getLine

getInputOrExit :: IO String
getInputOrExit = do
  inp <- getLineWithOutput "MSI> "
  case inp of
    "exit" -> do
       putStrLn "Bye-bye!"
       exitSuccess
    _      -> return inp

pprintType :: MSType -> String
pprintType t = case t of
  TInt -> "int"
  TString -> "string"
  TBool -> "bool"
  TArray t -> pprintType t ++ "[]"
  TMapping tk tv -> pprintType tk ++ " => " ++ pprintType tv
  TVoid -> "void"

pprintValue :: MSValue -> String
pprintValue v = case v of
  VNull _ -> "null"
  VInt n -> show n
  VString s -> show s
  VBool False -> "false"
  VBool True -> "true"
  VArray _ mvs -> "[" ++ pprintList mvs ++ "]"
  VMapping _ _ m -> "[" ++ pprintMap (toList m) ++ "]"
  VVoid -> "void"
  where
    pprintList [x] = pprintValue x
    pprintList (x:xs) = pprintValue x ++ ", " ++ pprintList xs
    pprintList [] = ""
    pprintMap [(x, y)] = pprintValue x ++ " -> " ++ pprintValue y
    pprintMap ((x, y):tl) =  pprintValue x ++ " -> " ++ pprintValue y ++ ", " ++ pprintMap tl
    pprintMap [] = ""
    
pprintError :: MSError -> String
pprintError (EInterpretation (RequireFailed s)) = "Reverted with: " ++ s
pprintError (EInterpretation e) = "Interpretation error: " ++ text
  where
    text = case e of
      NameNotExist id -> "Name not exist: " ++ id
      DuplicatedName id -> "Duplicated name: " ++ id
      UndefinedName id -> "Undefined name: " ++ id
      MismatchedType t1 t2 -> "Mismatched type: " ++ pprintType t1 ++ " and " ++ pprintType t2
      UnsupportedBinOp op t1 t2 -> "Unsupported binary operator: " ++ show op ++ " for types: " ++ pprintType t1 ++ ", " ++ pprintType t2
      UnsupportedUnOp op t -> "Unsupported unary operator: " ++ show op ++ " for type: " ++ pprintType t
      ZeroDivision -> "Zero division error"
      IndexOutOfBounds v -> "Index out of bounds: " ++ pprintValue v
      KeyNotFound v -> "Key not found: " ++ pprintValue v
      MismatchedArgsCount -> "Mismatched arguments count"
      Jump j -> case j of
        Break -> "Unexpected break statement"
        Continue -> "Unexpected continue statement"
        Return _ -> "Unexpected return statement"
      RequireFailed s -> undefined 
pprintError (ERuntime e) = "Runtime error: " ++ text
  where
    text = case e of
      EmptyCallStack -> "Empty call stack"
      EmptyScopeStack -> "Empty scope stack"

execRepl :: MSStatement -> MSRuntimeData -> (Either MSError MSValue, MSRuntimeData)
execRepl s = run (evalRepl s)

replLoop :: MSProgram -> IO ()
replLoop p = do

--  putStrLn $   " ____    ____   ______   _____ "
--  putStrLn $   "|_   \\  /   _|.' ____ \\ |_   _|"
--  putStrLn $   "  |   \\/   |  | (___ \\_|  | |  "
--  putStrLn $   "  | |\\  /| |   _.____`.   | |  "
--  putStrLn $   " _| |_\\/_| |_ | \\____) | _| |_ "
--  putStrLn $   "|_____||_____| \\______.'|_____|    Mini Solidity Interpreter"

  msData <- loadInitialData p
  loop msData
  where
    loop d = do
      inp <- getInputOrExit
      case parseReplStatement inp of
        Right repl -> do
          -- print repl
          let (res, newMsData) = run (evalRepl repl) d
          case res of
            Right value -> handleSuccess value newMsData
            Left err -> handleRuntimeError err d
        Left err -> handleParsingError err d

    handleSuccess :: MSValue -> MSRuntimeData -> IO ()
    handleSuccess v d = do
      let output = "(" ++ pprintType (typeOf v) ++ ") " ++ pprintValue v
      putStrLn output
      loop d

    handleRuntimeError :: MSError -> MSRuntimeData -> IO ()
    handleRuntimeError err d = do
      putStrLn $ pprintError err
      loop d

    handleParsingError :: ParseError -> MSRuntimeData -> IO ()
    handleParsingError err d = do
      putStrLn "Parsing error:"
      print err
      loop d


main :: IO ()
main = do
  fileName <- getFileName
  text <- readFile fileName
  program <- parse text
  replLoop program
