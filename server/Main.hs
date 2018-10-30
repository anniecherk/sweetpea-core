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

data SolutionSpec = SolutionSpec { assignment :: [Int]
                                 , frequency :: Int
                                 } deriving (Generic, Eq, Show, ToJSON, FromJSON)

data ResponseSpec = ResponseSpec { ok :: Bool
                                 , solutions :: [SolutionSpec]
                                 , stdout :: String
                                 , stderr :: String
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

     post "experiments/build-cnf" $ do
       spec <- jsonBody' :: ApiAction JSONSpec
       let dimacsStr = processRequests spec
       text $ pack dimacsStr

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
         then json $ ResponseSpec False [] stdout stderr
         else do
           solutionFileStr <- liftIO $ readSolutionFile outputFilename
           json $ ResponseSpec True (extractSolutions solutionFileStr) "" ""

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

strToInt :: Text -> Int
strToInt str = read $ unpack str

buildSolution :: Text -> SolutionSpec
buildSolution sol = do
  let intStrs = split (==' ') (pack [c | c <- unpack $ strip sol, not (c == 'v')])
    in let assignment = map strToInt (take ((length intStrs) - 1) intStrs)
           frequency = strToInt $ last $ split (==':') (last intStrs)
       in SolutionSpec assignment frequency
