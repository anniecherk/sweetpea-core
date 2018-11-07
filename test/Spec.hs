{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Trans.State
import Control.Monad (replicateM)

import DataStructures
import ServerHelpers

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [dataStructureTests, serverTests]

dataStructureTests :: TestTree
dataStructureTests = testGroup "DataStructure Tests" [helperTests]

serverTests :: TestTree
serverTests = testGroup "Server Tests" [strToIntTests, updateHeaderTests, parseCMSatSolutionTests]

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

---------------------------------------------------------------------------------------------------------------
-- ServerHelpers Tests

strToIntTests = testGroup "strToInt tests"
  [ testCase "should convert to int correctly" $
    strToInt "42" @?= 42
  ]

updateHeaderTests = testGroup "udpateHeader tests"
  [ testCase "should update the clause count" $
    updateHeader "p cnf 3 5" @?= "p cnf 3 6"
  ]

parseCMSatSolutionTests = testGroup "parseCMSatSolution tests"
  [ testCase "single line solution" $
    parseCMSatSolution "s SATISFIABLE\nv 1 2 -3 4 0" @?= [1, 2, -3, 4, 0]
  , testCase "two line solution" $
    parseCMSatSolution "s SATISFIABLE\nv 1 2 -3 4\nv -5 6 7 0\n" @?= [1, 2, -3, 4, -5, 6, 7, 0]

  , testCase "solution with extra whitespace" $
    let output = "stdout: s SATISFIABLE\n" ++
                 "v -1 -2 3 4 -5 \n" ++
                 "v -6 7 -8 9 \n" ++
                 "v 10 11 0  " in
      parseCMSatSolution output @?= [-1, -2, 3, 4, -5, -6, 7, -8, 9, 10, 11, 0]
  ]
