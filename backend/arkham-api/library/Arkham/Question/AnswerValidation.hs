module Arkham.Question.AnswerValidation (
  stripPromptWrappers,
  replayAmountsValid,
  replayPaymentAmountsValid,
  replayExchangeAmountsValid,
  replayExchangeAmountWithinBalances,
  replayAmountAllocationValid,
  amountTargetSatisfied,
) where

import Arkham.Id (InvestigatorId)
import Arkham.Prelude
import Arkham.Question (AmountChoice (..), AmountTarget (..), PaymentAmountChoice (..), Question (..))
import Arkham.Source (Source)
import Arkham.Token (Token)
import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID

stripPromptWrappers :: Question message -> Question message
stripPromptWrappers = \case
  QuestionLabel _ _ prompt -> stripPromptWrappers prompt
  PayCostQuestion _ prompt -> stripPromptWrappers prompt
  QuestionWithSource _ _ prompt -> stripPromptWrappers prompt
  prompt -> prompt

replayAmountsValid :: Map UUID.UUID Int -> Question message -> Bool
replayAmountsValid amounts prompt = case stripPromptWrappers prompt of
  ChooseAmounts _ target choices _ ->
    replayAmountAllocationValid
      [(choiceId, lowerBound, upperBound) | AmountChoice choiceId _ lowerBound upperBound <- choices]
      (Just target)
      amounts
  _ -> False

replayPaymentAmountsValid :: Map UUID.UUID Int -> Question message -> Bool
replayPaymentAmountsValid amounts prompt = case stripPromptWrappers prompt of
  ChoosePaymentAmounts _ target choices ->
    replayAmountAllocationValid
      [ (choiceId, lowerBound, upperBound)
      | PaymentAmountChoice choiceId _ lowerBound upperBound _ _ <- choices
      ]
      target
      amounts
  _ -> False

replayExchangeAmountsValid :: Source -> InvestigatorId -> InvestigatorId -> Token -> Int -> Question message -> Bool
replayExchangeAmountsValid answerSource answerFrom answerTo answerToken amount prompt =
  case stripPromptWrappers prompt of
    ChooseExchangeAmounts
      promptSource
      firstInvestigator
      firstAmount
      secondInvestigator
      secondAmount
      promptToken ->
        answerSource == promptSource
          && answerToken == promptToken
          && ( (answerFrom == firstInvestigator && answerTo == secondInvestigator)
                || (answerFrom == secondInvestigator && answerTo == firstInvestigator)
             )
          && replayExchangeAmountWithinBalances
            firstInvestigator
            firstAmount
            secondInvestigator
            secondAmount
            answerFrom
            answerTo
            amount
    _ -> False

replayExchangeAmountWithinBalances
  :: InvestigatorId
  -> Int
  -> InvestigatorId
  -> Int
  -> InvestigatorId
  -> InvestigatorId
  -> Int
  -> Bool
replayExchangeAmountWithinBalances iid1 iid1Amount iid2 iid2Amount fromIid toIid amount
  | iid1Amount < 0 || iid2Amount < 0 = False
  | fromIid == iid1 && toIid == iid2 =
      transferred <= toInteger iid1Amount
        && transferred >= negate (toInteger iid2Amount)
  | fromIid == iid2 && toIid == iid1 =
      transferred <= toInteger iid2Amount
        && transferred >= negate (toInteger iid1Amount)
  | otherwise = False
 where
  transferred = toInteger amount

replayAmountAllocationValid
  :: [(UUID.UUID, Int, Int)]
  -> Maybe AmountTarget
  -> Map UUID.UUID Int
  -> Bool
replayAmountAllocationValid choices target amounts =
  length choices == Map.size bounds
    && Map.keysSet amounts == Map.keysSet bounds
    && all validChoice choices
    && amountTargetSatisfied target (sum $ map (toInteger . snd) $ Map.toList amounts)
 where
  bounds = Map.fromList [(choiceId, (lowerBound, upperBound)) | (choiceId, lowerBound, upperBound) <- choices]
  validChoice (choiceId, lowerBound, upperBound) =
    lowerBound <= upperBound
      && let amount = Map.findWithDefault 0 choiceId amounts
          in amount >= lowerBound && amount <= upperBound

amountTargetSatisfied :: Maybe AmountTarget -> Integer -> Bool
amountTargetSatisfied target total = case target of
  Nothing -> True
  Just (MinAmountTarget minimumAmount) -> total >= toInteger minimumAmount
  Just (MaxAmountTarget maximumAmount) -> total <= toInteger maximumAmount
  Just (TotalAmountTarget requiredAmount) -> total == toInteger requiredAmount
  Just (AmountOneOf allowedAmounts) -> total `elem` map toInteger allowedAmounts
