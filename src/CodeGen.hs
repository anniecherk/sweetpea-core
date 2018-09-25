module CodeGen
(showDIMACS, showCNF)
where

import DataStructures
import Data.List
import Data.List.Split


showDIMACS :: CNF -> Int -> Int -> String
showDIMACS cnf nVars support = "p cnf " ++ show nVars ++ " " ++ show (length cnf)
  ++ "\n" ++ showIndependentSupport support
  ++ "\n" ++ showCNF cnf

showIndependentSupport :: Int -> String
showIndependentSupport support = intercalate "\n" (map prepareSupportLine (chunksOf 10 [1..support]))

prepareSupportLine :: [Int] -> String
prepareSupportLine vars =
  "c ind" ++ (foldl' (\str n -> str ++ " " ++ n) "" (map show vars)) ++ " 0"

showCNF :: CNF -> String
showCNF = foldl' (\acc andClause -> showOrClause andClause ++ acc) "" -- bizarre head/tail splitting because want no leading space

showOrClause :: [Int] -> String
showOrClause andClause = foldl' (\acc or1 -> acc ++ " " ++ show or1) (show $ head andClause) (tail andClause) ++ " 0\n"
