{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main where

import Web.Spock
import Web.Spock.Config

import Control.Monad.IO.Class
import GHC.Generics
import Data.Aeson hiding (json)
import Data.List
import Data.Monoid
import Data.Text (Text, pack, unpack, split, strip)
import Data.UUID
import Data.UUID.V4 as UUID

import System.Exit
import System.IO
import System.Process
import qualified System.IO.Strict as S

import Parser
import ServerHelpers

data SolutionSpec = SolutionSpec { assignment :: [Int]
                                 , frequency :: Int
                                 } deriving (Generic, Eq, Show, ToJSON, FromJSON)

data ResponseSpec = ResponseSpec { ok :: Bool
                                 , solutions :: [SolutionSpec]
                                 , count :: Int
                                 , stdout :: String
                                 , stderr :: String
                                 } deriving (Generic, Eq, Show, ToJSON, FromJSON)


type Api = SpockM () () () ()
type ApiAction a = SpockAction () () () a

serverCfg :: IO (SpockCfg () () ())
serverCfg = do
    spockConfig <- defaultSpockCfg () PCNoDatabase ()
    return spockConfig { spc_maxRequestSize = Nothing } -- Disable the request size limit

main :: IO ()
main = do
  spockCfg <- serverCfg
  runSpock 8080 (spock spockCfg app)

app :: Api
app =
  do get root $
       text "Healthy\n"

     post "experiments/build-cnf" $ do
       spec <- jsonBody' :: ApiAction JSONSpec
       let dimacsStr = processRequests spec
       text $ pack dimacsStr

     -- Doesn't work yet, need to get a statically-linked binary built into the docker image.
     post "experiments/count-solutions" $ do
       spec <- jsonBody' :: ApiAction JSONSpec
       guid <- liftIO $ toString <$> UUID.nextRandom
       let filename = guid ++ ".cnf"
       let outputFilename = guid ++ ".out"
       liftIO $ saveCnf filename spec

       -- Invoke sharpSAT to get an approximate solution count, time out at 60 seconds
       (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode "sharpSAT" ["-q", "-t", "60", filename] ""

       if exitCode == ExitSuccess
         then json $ ResponseSpec True [] (extractCount stdout) stdout stderr
         else json $ ResponseSpec False [] (-1) stdout stderr

     post "experiments/generate" $ do
       spec <- jsonBody' :: ApiAction JSONSpec
       guid <- liftIO $ toString <$> UUID.nextRandom
       let filename = guid ++ ".cnf"
       let outputFilename = guid ++ ".out"
       liftIO $ saveCnf filename spec

       let args = case (arguments (unigen spec)) of
             Nothing -> []
             Just argList -> argList
       (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode "unigen" (args ++ [filename, outputFilename]) ""

       -- Perusing the code, it appears that unigen uses an exit code of 0 to indicate error, while positive
       -- exit codes seem to indicate some form of success.
       -- https://bitbucket.org/kuldeepmeel/unigen/src/4677b2ec4553b2a44a31910db0037820abdc1394/ugen2/cmsat/Main.cpp?at=master&fileviewer=file-view-default#Main.cpp-831
       if exitCode == ExitSuccess
         then json $ ResponseSpec False [] (-1) stdout stderr
         else do
           solutionFileStr <- liftIO $ readSolutionFile outputFilename
           json $ ResponseSpec True (extractSolutions solutionFileStr) (-1) "" ""

     post ("experiments/generate/non-uniform" <//> var) $ \count -> do
       spec <- jsonBody' :: ApiAction JSONSpec
       guild <- liftIO $ toString <$> UUID.nextRandom
       let filename = guild ++ ".cnf"
       liftIO $ saveCnf filename spec

       solutions <- liftIO $ computeSolutions filename (support (unigen spec)) count []
       json $ ResponseSpec True (map ((flip SolutionSpec) 1) solutions) (-1) "" ""


saveCnf :: String -> JSONSpec -> IO ()
saveCnf filename spec =
  let dimacsStr = processRequests spec
    in writeFile filename dimacsStr

readSolutionFile :: String -> IO String
readSolutionFile filename = do
  readFile filename

extractSolutions :: String -> [SolutionSpec]
extractSolutions solutionStr =
  let lines = split (=='\n') (strip (pack solutionStr))
  in map buildSolution lines

buildSolution :: Text -> SolutionSpec
buildSolution sol = do
  let intStrs = split (==' ') (pack [c | c <- unpack $ strip sol, not (c == 'v')])
    in let assignment = map strToInt (take ((length intStrs) - 1) intStrs)
           frequency = strToInt $ last $ split (==':') (last intStrs)
       in SolutionSpec assignment frequency

extractCount :: String -> Int
extractCount output =
  let lines = split (=='\n') (strip (pack output))
  in strToInt $ lines !! 0

computeSolutions :: String -> Int -> Int -> [[Int]] -> IO [[Int]]
computeSolutions filename support count solutions = do
  if count == 0
    then return solutions
    else do
    -- Invoke cryptominisat to get a solution
    (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode "cryptominisat5" ["--verb=0", filename] ""

    -- Extract assignment from stdout
    let rawSolution = parseCMSatSolution stdout
    let solution = take support rawSolution

    -- Update the file to not include this solution.
    liftIO $ updateFile filename solution

    -- Go for another round
    computeSolutions filename support (count - 1) (solutions ++ [solution])

updateFile :: String -> [Int] -> IO ()
updateFile filename solution = do
  -- Load the CNF file, split into lines
  -- Read strictly to ensure we can rewrite the file afterwards.
  contents <- liftIO $ S.readFile filename
  let lines = split (=='\n') (strip (pack contents))

  -- Update the clause count.
  let updatedHeader = updateHeader (unpack (lines !! 0))

  -- Negate the given solution
  let negatedSolution = map (*(-1)) solution
  let negatedSolutionStr = unwords $ map show (negatedSolution ++ [0])

  -- Append it to the end of the CNF file, with a 0 at the end.
  let updatedLines = [(pack updatedHeader)] ++ (drop 1 lines) ++ [(pack negatedSolutionStr)]

  -- Rewrite file to disk.
  let updatedContents = (intercalate "\n" (map unpack updatedLines))
  writeFile filename updatedContents
