{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Actus (actusMain) where

import Control.Monad
import Data.Maybe
import Data.Ratio
import Data.Time
import GHC.Generics (Generic)
import qualified Money.Amount as Amount
import qualified Money.Amount as Money
import Money.QuantisationFactor
import Numeric.Natural
import System.Environment
import System.Exit
import Text.Printf
import Text.Read

actusMain :: IO ()
actusMain = do
  args <- getArgs
  case args of
    [totalPrincipalStr, interestRateStr, repaymentAmountStr, startDayStr, yearsStr] -> do
      totalPrincipal <- case readMaybe totalPrincipalStr >>= Amount.fromDouble quantisationFactor of
        Nothing -> die "Could not read totalPrincipal"
        Just a -> pure a
      interestRate <- case readMaybe interestRateStr of
        Nothing -> die "Could not read interest rate percentage"
        Just ir -> pure (ir % 100)
      repaymentAmount <- case readMaybe repaymentAmountStr >>= Amount.fromDouble quantisationFactor of
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
  putStrLn "Analysing annuity with"
  putStrLn $ unwords ["Start day:", show startDay]
  putStrLn $ unwords ["Principal amount:", formatUSD $ annuityPrincipal annuity]
  putStrLn $ unwords ["Interest rate per year:", printf "%.2f %%" $ (realToFrac :: Ratio Natural -> Double) $ annuityInterestRate annuity * 100]
  putStrLn ""
  putStrLn $ unwords ["Analysing based on variable maturity and given amount of", formatUSD repaymentAmount]
  case calculateFloatingMaturity annuity repaymentAmount startDay of
    Nothing -> die "Cannot be repaid with the given repayment amount"
    Just payments -> printPayments payments
  putStrLn ""
  putStrLn $ unwords ["Analysing based on variable amount and given maturity of", show periods, "periods."]
  mapM_ printPayments $ calculateFloatingAmount annuity periods startDay

printPayments :: [(Day, (Ratio Natural, Money.Amount, Money.Amount, Money.Amount))] -> IO ()
printPayments payments = do
  forM_ payments $ \(day, (rateDuringThisPeriod, interest, principal, principalLeftover)) -> do
    let totalRepaid = partialAdd interest principal
    putStrLn $
      unwords
        [ "Payment on: " <> show day,
          " Rate during this period",
          printf "%.6f" ((realToFrac :: Ratio Natural -> Double) rateDuringThisPeriod),
          " Interest paid:",
          formatUSD interest,
          " Principal repaid:",
          formatUSD principal,
          " Total paid:",
          formatUSD totalRepaid,
          " Principal leftover:",
          formatUSD principalLeftover
        ]
  putStrLn $ unwords ["Total number of payments:", show (length payments)]
  putStrLn $ unwords ["Total interest:", maybe "" formatUSD $ Amount.sum (map (\(_,(_,i,_,_)) -> i) payments)]

formatUSD :: Money.Amount -> String
formatUSD a = printf "%10s %s" (Amount.format quantisationFactor a) "USD"

quantisationFactor :: QuantisationFactor
quantisationFactor = QuantisationFactor 100

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

-- [(Date of payment, interest paid, principal paid)]
calculateFloatingMaturity :: Annuity -> Money.Amount -> Day -> Maybe [(Day, (Ratio Natural, Money.Amount, Money.Amount, Money.Amount))]
calculateFloatingMaturity Annuity {..} repaymentAmount beginDay = go beginDay annuityPrincipal
  where
    go :: Day -> Money.Amount -> Maybe [(Day, (Ratio Natural, Money.Amount, Money.Amount, Money.Amount))]
    go lastDay currentPrincipal =
      if currentPrincipal == Amount.zero
        then Just []
        else
          let currentDay = calculateNextMonth lastDay
              daysBetween = fromIntegral $ diffDays currentDay lastDay
              (y, _, _) = toGregorian lastDay
              yearLength = if isLeapYear y then 366 else 365
              periodInterestRate = annuityInterestRate * (daysBetween % yearLength)
              -- Interest amount = current principal amount * interest rate
              (interestAmount, _) = partialFraction Money.RoundDown currentPrincipal periodInterestRate
           in -- (if actualRate /= annuityInterestRate then traceShow ("Rates differ slightly", actualRate, annuityInterestRate, realToFrac actualRate - realToFrac annuityInterestRate :: Double) else id) $
              -- If the interest amonut is more than how much we pay per month, then the loan can never be repayed
              if interestAmount >= repaymentAmount
                then -- error $
                --   unwords
                --     [ "Can never repay the loan because the interest exceeds the repayment amount.",
                --       show interestAmount,
                --       ">=",
                --       show repaymentAmount
                --     ]
                  Nothing
                else -- The amount that can go towards the principal is the difference between the total and the amonut that goes to interest
                -- Principal amount = Repayment amount - Interest amount

                  let principalAmount = partialSubtract repaymentAmount interestAmount
                   in -- If the amount that can go towards the principal is more than the amount of principal leftover
                      -- then this is the last payment
                      if principalAmount >= currentPrincipal -- Payment done
                        then Just [(currentDay, (periodInterestRate, interestAmount, currentPrincipal, Amount.zero))]
                        else do
                          -- Otherwise we make one payment
                          -- Principal amount leftover = current principal amount - amount that goes towards principal

                          let leftoverPrincipal = partialSubtract currentPrincipal principalAmount
                          restPayments <- go currentDay leftoverPrincipal
                          Just $ (currentDay, (periodInterestRate, interestAmount, principalAmount, leftoverPrincipal)) : restPayments

partialSubtract :: Money.Amount -> Money.Amount -> Money.Amount
partialSubtract a b = case Amount.subtract a b of
  Nothing -> error "Could not subtract"
  Just c -> c

partialAdd :: Money.Amount -> Money.Amount -> Money.Amount
partialAdd a b = case Amount.add a b of
  Nothing -> error "Could not add"
  Just c -> c

calculateNextMonth :: Day -> Day
calculateNextMonth d =
  let (y, m, dn) = toGregorian d
   in if m == 12
        then fromGregorian (y + 1) 1 dn
        else fromGregorian y (m + 1) dn

calculateFloatingAmount :: Annuity -> Word -> Day -> Maybe [(Day, (Ratio Natural, Money.Amount, Money.Amount, Money.Amount))]
calculateFloatingAmount annuity@Annuity {..} periods startDay =
  let ir_i :: [Ratio Natural]
      ir_i = goPeriods 0 startDay
        where
          goPeriods :: Word -> Day -> [Ratio Natural]
          goPeriods i lastDay
            | i >= periods = []
            | otherwise =
                let (y, _, _) = toGregorian lastDay
                    yearLength = if isLeapYear y then 366 else 365
                    currentDay = calculateNextMonth lastDay
                 in goPeriods (succ i) currentDay
                      ++ [annuityInterestRate * (fromIntegral (diffDays currentDay lastDay) % yearLength)]

      i_i :: Word -> Ratio Natural
      i_i i = product $ map (1 +) (take (fromIntegral i) ir_i)

      (amount, _) = partialFraction Money.RoundUp annuityPrincipal (i_i periods / sum [i_i i | i <- [0 .. periods - 1]])
   in -- Let A_N be the outstanding debt after N months. Then
      -- A_(N+1) = A_N * (1 + IR_N) - P
      -- where IR_N is the interest rate during month N (dependent on the number of days in month N) and P is the payment (which should be the same all months). Iteratively, we then see that
      -- A_N = A_0 * prod{i=0}^{N-1} (1 + IR_i) - P * sum_{i=0}_{N-1} prod_{j=0}_i (1 + IR_j)
      -- Let I_i = prod_{j=0}^i (1 + IR_j), then this becomes
      -- A_N = A_0 * I_N - P * sum_{i=0}_{N-1} I_i.
      -- We then say A_N = 0, for a given N, and attempt to calculate P. As we can see from the last formula,
      -- P = A_0 * I_N / sum_{i=0}_{N-1} I_i.
      calculateFloatingMaturity annuity amount startDay

partialFraction ::
  Money.Rounding ->
  Money.Amount ->
  Ratio Natural ->
  (Money.Amount, Ratio Natural)
partialFraction ro a r =
  let (ma, ra) = Amount.fraction ro a r
   in (fromJust ma, ra)
