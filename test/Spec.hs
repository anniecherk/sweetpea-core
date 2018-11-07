{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Trans.State
import Control.Monad (replicateM)

import DataStructures

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [dataStructureTests]

dataStructureTests :: TestTree
dataStructureTests = testGroup "DataStructure Tests" [helperTests]

---------------------------------------------------------------------------------------------------------------
-- DataStructures tests

helperTests = testGroup "var/cnf utility tests"
  [ testCase "getFresh" $
      runState getFresh emptyState
        @?= (1,(1,[]))
  , testCase "getNFresh" $
      runState (getNFresh 3) emptyState
        @?= ([1,2,3],(3,[]))
  , testCase "nested getNFresh" $
      runState (getNFresh 3) (execState (getNFresh 3) emptyState)
        @?= ([4,5,6],(6,[]))
  , testCase "appendCNF" $
      execState (appendCNF [[1, 2, -3]]) emptyState
        @?= (0,[[1,2,-3]])
  , testCase "nested appendCNF" $
      execState (appendCNF [[-4, 5], [4]]) (execState (appendCNF [[1, 2, -3]]) emptyState)
        @?= (0,[[-4,5],[4],[1,2,-3]])
  , testCase "zeroOut" $
        execState (zeroOut [1, 2, 3]) emptyState
          @?= (0,[[-1],[-2],[-3]])
  , testCase "setToOne" $
        execState (setToOne 1) emptyState
          @?= (0,[[1]])
  , testCase "setToOne + zeroOut nested" $
        execState (zeroOut [1, 2, 3]) (execState (setToOne 4) emptyState)
          @?= (0,[[-1],[-2],[-3],[4]])
  , testCase "setToZero" $
        execState (setToZero 1) emptyState
          @?= (0,[[-1]])
  ]
