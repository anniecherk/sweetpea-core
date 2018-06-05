module Main where

import Data.ByteString.Lazy
import Data.Aeson

import CodeGen
import DataStructures
import Parser


main :: IO ()
main = do contents <- Data.ByteString.Lazy.readFile "ll_constraints.json"
          let instructions = decode contents :: Maybe JSONSpec
          case instructions of
              Nothing -> error "misformatted requests file"
              Just spec -> Prelude.putStrLn $ processRequests spec
