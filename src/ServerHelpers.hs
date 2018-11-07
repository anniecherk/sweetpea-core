module ServerHelpers where

import Data.List
import Data.Text (Text, pack, unpack, split, strip)

strToInt :: Text -> Int
strToInt str = read $ unpack str

updateHeader :: String -> String
updateHeader header =
  let segments = split (==' ') (strip (pack header))
  in let newClauseCount = (strToInt (segments !! 3)) + 1
     in unwords $ map unpack ((take 3 segments) ++ [pack $ show newClauseCount])

parseCMSatSolution :: String -> [Int]
parseCMSatSolution output = do
  -- Get solution lines from output.
  let lines = filter (isPrefixOf "v") (map (unpack . strip) (split (=='\n') (strip (pack output))))
    in let intStrs = split (==' ') $ strip $ pack $ (filter (/='v') (foldr (++) "" lines))
       in map strToInt intStrs
