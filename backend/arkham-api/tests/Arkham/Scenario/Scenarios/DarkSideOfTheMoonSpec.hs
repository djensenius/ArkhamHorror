module Arkham.Scenario.Scenarios.DarkSideOfTheMoonSpec (spec) where

import Arkham.Campaigns.TheDreamEaters.Key (TheDreamEatersKey (RandolphWasCaptured))
import TestImport.New

spec :: Spec
spec = describe "Dark Side of the Moon" do
  it "starts when Randolph was captured but was not added to a deck"
    . scenarioTest "06206"
    $ \_ -> do
      record RandolphWasCaptured
      pushAndRun PreScenarioSetup
