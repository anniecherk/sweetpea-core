{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Parser

where

import Data.Aeson
import GHC.Generics
import Control.Monad.Trans.State
import DataStructures
import SweetPeaCore
import CodeGen


data Request = Request { equalityType :: Ordering
                       , k :: Int
                       , booleanValues :: [Int]
                       } deriving (Generic, Eq, Show, ToJSON, FromJSON)

data UnigenOptions = UnigenOptions { support :: Int
                                   , arguments :: Maybe [String]
                                   } deriving (Generic, Eq, Show, ToJSON, FromJSON)

data JSONSpec = JSONSpec { fresh :: Int
                         , cnfs :: CNF
                         , requests :: [Request]
                         , unigen :: UnigenOptions
                         } deriving (Generic, Eq, Show, ToJSON, FromJSON)


processRequests :: JSONSpec -> String
processRequests spec = showDIMACS finalCnf finalNVars (support (unigen spec))
  where (finalNVars, finalCnf) = (finalNVars, cnf ++ (cnfs spec))
          where (finalNVars, cnf) = execState (mapM processARequest (requests spec)) $ initState (fresh spec)

processARequest :: Request -> State (Count, CNF) ()
processARequest (Request EQ k boolVals) = assertKofN    k boolVals
processARequest (Request LT k boolVals) = kLessThanN    k boolVals
processARequest (Request GT k boolVals) = kGreaterThanN k boolVals
