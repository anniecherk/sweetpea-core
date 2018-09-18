{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main where

import Web.Spock
import Web.Spock.Config

import Control.Monad.IO.Class
import GHC.Generics
import Data.Aeson hiding (json)
import Data.Monoid
import Data.Text (Text, pack, unpack, split, strip)
import Data.UUID
import Data.UUID.V4 as UUID

import System.Exit
import System.IO
import System.Process

import Parser

data ResponseSpec = ResponseSpec { solutions :: [[Int]]
                                 } deriving (Generic, Eq, Show, ToJSON, FromJSON)

type Api = SpockM () () () ()
type ApiAction a = SpockAction () () () a

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: Api
app =
  do get root $
       text "Healthy\n"

     post "experiments/generate" $ do
       spec <- jsonBody' :: ApiAction JSONSpec
       guid <- liftIO $ toString <$> UUID.nextRandom
       let filename = guid ++ ".cnf"
       liftIO $ saveCnf filename spec
       solutionFilename <- liftIO $ runUnigen filename guid
       solutionContents <- liftIO $ readSolutionFile solutionFilename
       let solutions = extractSolutions solutionContents
       json $ ResponseSpec solutions

saveCnf :: String -> JSONSpec -> IO ()
saveCnf filename spec =
  let dimacsStr = processRequests spec
    in writeFile filename dimacsStr

runUnigen :: String -> String -> IO String
runUnigen filename guid = do
  let outputFilename = guid ++ ".out"
  (exitCode, _, _) <- readProcessWithExitCode "unigen" ["--verbosity=0", filename, outputFilename] ""

  -- Perusing the code, it appears that unigen uses an exit code of 0 to indicate error, while positive
  -- exit codes seem to indicate some form of success.
  -- https://bitbucket.org/kuldeepmeel/unigen/src/4677b2ec4553b2a44a31910db0037820abdc1394/ugen2/cmsat/Main.cpp?at=master&fileviewer=file-view-default#Main.cpp-831
  if exitCode == ExitSuccess
    then error "Either something went wrong while running unigen, or the formula was unsatisfiable"
    else return outputFilename

readSolutionFile :: String -> IO String
readSolutionFile filename = do
  readFile filename

extractSolutions :: String -> [[Int]]
extractSolutions solutionStr =
  let lines = split (=='\n') (strip (pack solutionStr))
  in map buildSolution lines

buildSolution :: Text -> [Int]
buildSolution sol = do
  let intStrs = split (==' ') (pack [c | c <- unpack $ strip sol, not (c == 'v')])
  map (\s -> read $ unpack s :: Int) (take ((length intStrs) - 1) intStrs)
