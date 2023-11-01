{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Actus (actusMain) where

import Control.Monad
import Data.Ratio
import Data.Time
import Data.Word
import Debug.Trace
import GHC.Generics (Generic)
import qualified Money.Amount as Amount
import qualified Money.Amount as Money
import Numeric.Natural
import System.Environment
import System.Exit
import Text.Printf
import Text.Read
import Text.Show.Pretty (pPrint)

actusMain :: IO ()
actusMain = do
  args <- getArgs
  case args of
    [totalPrincipalStr, interestRateStr, repaymentAmountStr, startDayStr, yearsStr] -> do
      totalPrincipal <- case readMaybe totalPrincipalStr >>= Amount.fromDouble minimalQuantisations of
        Nothing -> die "Could not read totalPrincipal"
        Just a -> pure a
      interestRate <- case readMaybe interestRateStr of
        Nothing -> die "Could not read interest rate percentage"
        Just ir -> pure (ir % (100 * 12))
      repaymentAmount <- case readMaybe repaymentAmountStr >>= Amount.fromDouble minimalQuantisations of
        Nothing -> die "Could not read repaymentAmount"
        Just a -> pure a
      startDay <- case parseTimeM True defaultTimeLocale "%F" startDayStr of
        Nothing -> die "Could not read start day"
        Just d -> pure d
      let annuity = Annuity {annuityPrincipal = totalPrincipal, annuityInterestRate = interestRate}
      periods <- case readMaybe yearsStr of
        Nothing -> die "Could not read years"
        Just d -> pure (d * 12)
      annuityAnalysis annuity repaymentAmount startDay periods
    _ -> die "Usage: actus <totalPrincipal> <interestRatePercentage> <repaymentAmount> <startDay> <years>"

annuityAnalysis :: Annuity -> Money.Amount -> Day -> Word -> IO ()
annuityAnalysis annuity repaymentAmount startDay periods = do
  pPrint annuity
  putStrLn "Analysing annuity based on given amount and variable maturity"
  printPayments $ calculateFloatingMaturity annuity repaymentAmount startDay
  putStrLn "Analysing annuity based on given maturity and variable maturity"
  printPayments $ calculateFloatingAmount annuity periods startDay

printPayments :: [(Day, (Money.Amount, Money.Amount, Money.Amount))] -> IO ()
printPayments payments = do
  forM_ payments $ \(day, (interest, principal, principalLeftover)) -> do
    let totalRepaid = partialAdd interest principal
    putStrLn $ unwords ["Payment on: " <> show day, " Interest paid:", formatUSD interest, " Principal repaid:", formatUSD principal, " Total paid:", formatUSD totalRepaid, " Principal leftover:", formatUSD principalLeftover]
  putStrLn $ unwords ["Total number of payments:", show (length payments)]

formatUSD :: Money.Amount -> String
formatUSD = formatAmount minimalQuantisations "USD"

minimalQuantisations :: Word32
minimalQuantisations = 100

formatAmount :: Word32 -> String -> Money.Amount -> String
formatAmount minimalQuantisations symbol a = printf ("%10.2f " <> symbol) (Amount.toDouble minimalQuantisations a)

-- | Annuity
--
-- Annuity always has:
--
-- * Constant monthly payment
-- * Interest rate (can assume constant)
-- * Periodic repayment, paying interest first and principal with the rest
--
--
-- Two options for calculations:
--
-- * Fixed period, calculated amounts, all but the last will be constant
-- * Fixed amount, calculated period,
data Annuity = Annuity
  { annuityPrincipal :: Money.Amount,
    annuityInterestRate :: Ratio Natural -- Per period
  }
  deriving (Show, Eq, Generic)

exampleAnnuity :: Annuity
exampleAnnuity =
  Annuity
    { annuityPrincipal = Amount.fromMinimalQuantisations $ 100_000, -- 100K
      annuityInterestRate = (4 % 100) / 12
    }

-- [(Date of payment, interest paid, principal paid)]
calculateFloatingMaturity :: Annuity -> Money.Amount -> Day -> [(Day, (Money.Amount, Money.Amount, Money.Amount))]
calculateFloatingMaturity Annuity {..} repaymentAmount beginDay = go (calculateNextDay beginDay) annuityPrincipal
  where
    go currentDay currentPrincipal =
      let -- Interest amount = current principal amount * interest rate
          (interestAmount, actualRate) = Amount.fraction currentPrincipal annuityInterestRate
       in -- (if actualRate /= annuityInterestRate then traceShow ("Rates differ slightly", actualRate, annuityInterestRate, realToFrac actualRate - realToFrac annuityInterestRate :: Double) else id) $
          -- If the interest amonut is more than how much we pay per month, then the loan can never be repayed
          if interestAmount >= repaymentAmount
            then error "Can never repay the loan because the interest exceeds the repayment amount."
            else -- The amount that can go towards the principal is the difference between the total and the amonut that goes to interest
            -- Principal amount = Repayment amount - Interest amount

              let principalAmount = partialSubtract repaymentAmount interestAmount
               in -- If the amount that can go towards the principal is more than the amount of principal leftover
                  -- then this is the last payment
                  if principalAmount >= currentPrincipal -- Payment done
                    then [(currentDay, (interestAmount, currentPrincipal, Amount.zero))]
                    else -- Otherwise we make one payment
                    -- Principal amount leftover = current principal amount - amount that goes towards principal

                      let leftoverPrincipal = partialSubtract currentPrincipal principalAmount
                       in (currentDay, (interestAmount, principalAmount, leftoverPrincipal))
                            : go (calculateNextDay currentDay) leftoverPrincipal

partialSubtract :: Money.Amount -> Money.Amount -> Money.Amount
partialSubtract a b = case Amount.subtract a b of
  Nothing -> error "Could not subtract"
  Just c -> c

partialAdd :: Money.Amount -> Money.Amount -> Money.Amount
partialAdd a b = case Amount.add a b of
  Nothing -> error "Could not add"
  Just c -> c

calculateNextDay :: Day -> Day
calculateNextDay d =
  let (y, m, dn) = toGregorian d
   in if m == 12
        then fromGregorian (y + 1) 1 dn
        else fromGregorian y (m + 1) dn

calculateFloatingAmount :: Annuity -> Word -> Day -> [(Day, (Money.Amount, Money.Amount, Money.Amount))]
calculateFloatingAmount annuity periods startDay = []
