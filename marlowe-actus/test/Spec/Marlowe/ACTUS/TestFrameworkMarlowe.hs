{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Spec.Marlowe.ACTUS.TestFrameworkMarlowe
  ( tests
  )
  where

import Control.Monad.Reader (Reader, ask, runReader)
import Data.Time (LocalTime (..))
import GHC.Records (getField)
import Language.Marlowe.ACTUS.Domain.BusinessEvents
import Language.Marlowe.ACTUS.Domain.ContractTerms hiding (Assertion)
import Language.Marlowe.ACTUS.Domain.Ops
import Language.Marlowe.ACTUS.Domain.Schedule
import Language.Marlowe.ACTUS.Generator.Analysis
import Language.Marlowe.ACTUS.Generator.MarloweCompat (toMarlowe)
import Language.Marlowe.ACTUS.Model.ContractSchedule as S (maturity, schedule)
import Language.Marlowe.ACTUS.Model.StateTransition (CtxSTF (..))
import Spec.Marlowe.ACTUS.TestFramework hiding (tests)
import Test.Tasty
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)
import Text.Printf (printf)

tests :: String -> [TestCase] -> TestTree
tests n t =
  testGroup
    n
    [testCase (getField @"identifier" tc) (runTest tc {terms = setDefaultContractTermValues (terms tc)}) | tc <- t]
  where
    runTest :: TestCase -> Assertion
    runTest tc@TestCase {..} =
      let cashFlows =
            runReader
              (run tc)
              $ CtxSTF
                (toMarlowe terms)
                (calculationDay <$> schedule FP terms)
                (calculationDay <$> schedule PR terms)
                (calculationDay <$> schedule IP terms)
                (S.maturity terms)
                defaultRiskFactors

       in assertTestResults cashFlows results

    assertTestResults :: (RoleSignOps a, ScheduleOps a, YearFractionOps a) =>
      [CashFlowPoly a] -> [TestResult] -> IO ()
    assertTestResults [] []               = return ()
    assertTestResults (cf : cfs) (r : rs) = assertTestResult cf r >> assertTestResults cfs rs
    assertTestResults _ _                 = assertFailure "Sizes differ"

    assertTestResult :: CashFlowPoly a -> TestResult -> IO ()
    assertTestResult CashFlowPoly {..} TestResult {eventDate, eventType} = do
      assertEqual cashEvent eventType
      assertEqual cashPaymentDay eventDate
      -- assertEqual (reduce amount) (constnt payoff)
      where
        assertEqual a b = assertBool (err a b) $ a == b
        err a b = printf "Mismatch: actual %s, expected %s" (show a) (show b)

defaultRiskFactors :: ActusOps a => EventType -> LocalTime -> RiskFactorsPoly a
defaultRiskFactors _ _ =
  RiskFactorsPoly
    { o_rf_CURS = _one,
      o_rf_RRMO = _one,
      o_rf_SCMO = _one,
      pp_payoff = _zero,
      xd_payoff = _zero,
      dv_payoff = _zero
    }

run :: (RoleSignOps a, ScheduleOps a, YearFractionOps a) =>
  TestCase -> Reader (CtxSTF a) [CashFlowPoly a]
run TestCase {..} = do
  ctx <- ask
  pof <- genProjectedPayoffs
  let schedCfs = genCashflow (contractTerms ctx) <$> pof
  return $ maybe schedCfs (\d -> filter ((<= d) . cashCalculationDay) schedCfs) to
