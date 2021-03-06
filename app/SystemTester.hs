module Main where

import System.Environment
import System.Directory
import Data.Char
import Data.List.Split

import SweetPeaCore
import Testers
import DataStructures





-- TODO: refactor so doTestPopCount and doTestKofN are a single function
-- function signature polymorphism problem
doTestPopCount :: String -> IO String
doTestPopCount file = do
  result <- readFile file
  let shortFilePath = splitOn "/" file !! 2 --- a plague on both your houses
  let nSetVars = read (head $ splitOn "_" shortFilePath) ::Int
  return $ validate $ testResultPopCount result nSetVars

--------

printErrors :: [(String, FilePath)] -> [String]
printErrors = map (\(x, y) -> show y ++ " had error: " ++ x)

-- this is either (==) or (<)
doTestKandN :: (Int -> Int -> Bool) -> String -> IO String
doTestKandN eqOrLessThan file = do
  result <- readFile file
  let nSetVars = read (splitOneOf "_./" file !! 1) ::Int --- lord have mercy lord have mercy lord have mercy
  let k        = read (splitOneOf "_./" file !! 3) ::Int -- this makes assumptions about how the generateKofN formats file names...
  return $ validate $  testResultKandN result k nSetVars eqOrLessThan


processNandKTesting :: [String] -> (Int -> Int -> Bool) -> String -> IO ()
processNandKTesting args eqOrLessThan dirName = do
  fileList <- getDirectoryContents dirName
  results <- mapM (doTestKandN eqOrLessThan . (dirName ++)) $ drop 2 fileList
  if length args == 2 && (args !! 1) == "v"
  then mapM_ putStrLn results
  else mapM_ putStrLn $ printErrors $ filter ((/="Correct!") . fst) $ zip results $ drop 2 fileList


-- processNandKGeneration args popCountAllKlessthanNDIMACS "KlessthanNTests/"

processNandKGeneration :: [String] -> (Int -> [String]) -> String -> IO ()
processNandKGeneration args whichGenerator dirName = do
  let n = if length args == 2
          then read (head (tail args)) :: Int
          else 2                                                                        -- zipping index for file names
          -- TODO: refactor to zipWithM_ BUG: should be zipping with 0 if == & with 1 if < (bc can't have < 0 true)
  mapM_ (\(i, x) -> writeFile (dirName ++ show n ++ "_of_" ++ show i ++ ".cnf") x) $ zip [1..] $ whichGenerator n
  putStrLn "Done generating tests"





validate :: SATResult -> String
validate Correct = "Correct!"
validate Unsatisfiable = "oh no, Unsatisfiable!"
validate (WrongResult x y) = "expected " ++ show x ++ " but got " ++ show y
validate ParseError = "oh no, parse error!"

splitOnArgs :: [String] -> IO ()
splitOnArgs args

-- mean to be run with the gen_popcount_results.sh script: it generates files, runs them, reads them back in to be validated
-- example useage: gen_popcount_results.sh 6
-- 6 specifies the length of the sequence we're exhausitvely validating
-- defaults to 2
-- popcount is tested by setting the inputs, and validating what they add to!
  | head args == "generatePopCount"
  = do
      let popCountLength = if length args == 2
                           then read (head (tail args)) :: Int
                           else 2                                                                        -- zipping index for file names
      mapM_ (\(i, x) -> writeFile ("popCountTests/" ++ show popCountLength ++ "_popCounter" ++ "_" ++ show i ++ ".cnf") x) $ zip [0..] $ popCountDIMACS popCountLength
      putStrLn "Done generating tests"
-----
  | head args == "testPopCount"
  = do
      fileList <- getDirectoryContents "./popCountResults"
      -- tail . tail gets rid of . & .. directories
      results <- mapM (doTestPopCount . ("./popCountResults/" ++)) $ drop 2 fileList
      if length args == 2 && (args !! 1) == "v"
      then mapM_ putStrLn results
      else mapM_ putStrLn $ filter (/="Correct!") results
      putStrLn "Done testing"

------------------------------------------------------------------------------------------------------------------------
-- mean to be run with the gen_popcount_results.sh script: it generates files, runs them, reads them back in to be validated
-- example useage: gen_k_of_n_results.sh 6
-- 6 specifies the length of the sequence we're exhausitvely validating
-- defaults to 2
-- popcount is tested by setting the inputs, and validating what they add to!
  | head args == "generateKofN" = processNandKGeneration args assertAllKofNDIMACS "KofNTests/"

  | head args == "generateKlessthanN" = processNandKGeneration args popCountAllKlessthanNDIMACS "KlessthanNTests/"

-----
  | head args == "testKofN" = processNandKTesting args (==) "KofNResults/"
-----
  | head args == "testKlessthanN" = processNandKTesting args (<) "KlessthanNResults/"






--------------------------------------------------------------------------------
  --(they have hard-coded sol's: I checked these by hand early on)
  -- useage: ./run_tests.sh halfAdder
  | head args == "halfAdder"
  = mapM_ (\(i, x) -> writeFile ("generated_tests/halfAdder_" ++ show i ++ ".cnf") x) $ zip [0..] testHalfAdderDIMACS

------------------------------------------------------------------------------------------------------------------------
-- meant to be run from the run_tests.sh script: it generates all the files, and then diffs them against the SAT result
-- useage: ./run_tests.sh fullAdder
  | head args == "fullAdder" --tests ALL inputs to the full adder
  = do                                                                    -- zipping index for file names
    mapM_ (\(i, x) -> writeFile ("generated_tests/fullAdder_" ++ show i ++ ".cnf") x) $ zip [0..] testFullAdderDIMACS
    mapM_ (\(i, x) -> writeFile ("generated_tests/fullAdder_" ++ show i ++ ".sol") x) $ zip [0..] solnFullAdder

------------------------------------------------------------------------------------------------------------------------
-- meant to be run from the run_tests.sh script: it generates all the files, and then diffs them against the SAT result
-- useage: ./rippleCarry.sh --ripplecarry 6 // where 6 specifies the length of the sequenece we're exhaustively testing
-- defaults to size 2.. not sure if this is the best design but...
  -- | head args == "rippleCarry" --tests ALL inputs to the full adder
  -- = do
  --   let rippleSize = if length args == 2
  --                    then read (head (tail args)) :: Int
  --                    else 2                                                                     -- zipping index for file names
  --   mapM_ (\(i, x) -> writeFile ("generated_tests/rippleAdder" ++ show rippleSize ++ "_" ++ show i ++ ".cnf") x) $ zip [0..] $ testRippleCarryDIMACS rippleSize
  --   mapM_ (\(i, x) -> writeFile ("generated_tests/rippleAdder" ++ show rippleSize ++ "_" ++ show i ++ ".sol") x) $ zip [0..] $ solnRippleCarry rippleSize



--------------------------------------------------------------------------------
  | otherwise = putStrLn $ "I don't know what you want from me! \n \
  \You said: " ++ show args ++
  "\nTry one of these cmdln args:\n\
  \generatePopCount [2], testPopCount [v], \n\
  \generateKofN [2], testKofN [v], \n \
  \halfAdder, fullAdder, rippleCarry [2] \n"



main :: IO ()
main = do
        args <- getArgs
        if length args < 1
        then putStrLn "use commandline arg <generate n> to generate popCount tests of strings of length n \n or  commandline arg <test> to test all files in popCountResults directory."
        else splitOnArgs args
