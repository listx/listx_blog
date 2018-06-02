module Main where

import qualified Data.Map as M
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import GameButtonSeq

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProps, unitTests]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "GameButtonSeq (empty DB cannot produce matches)" $
    \buttonHist -> onButtons (buttonHist :: ButtonSeq) M.empty == []
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "GameButtonSeq (entry of multiple sequences with same name)" $
    (onButtons ["up", "punch"]
    $ register (["up", "punch"], "uppercut_2") buttonSeqDB)
      @?=
      ["uppercut_2", "uppercut"]
  , testCase "GameButtonSeq (no sequence found)" $
    onButtons
      ["down", "down", "forward", "punch", "down"]
      buttonSeqDB
      @?=
      []
  , testCase "GameButtonSeq (one sequences found)" $
    onButtons
      ["down", "down", "up", "punch"]
      buttonSeqDB
      @?=
      ["uppercut"]
  , testCase "GameButtonSeq (multiple sequences found)" $
    onButtons
      ["down", "down", "forward", "punch"]
      buttonSeqDB
      @?=
      ["hadoken", "charger"]
  ]
  where
  -- Default DB of button sequences.
  buttonSeqDB :: M.Map ButtonSeq [Name]
  buttonSeqDB = M.fromList
    [ (["down", "forward", "punch"], ["hadoken"])
    , (["forward", "punch"], ["charger"])
    , (["up", "punch"], ["uppercut"])
    ]
