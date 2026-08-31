{- | Thin wrapper around 'Api.Arkham.AwsEnvSupervisor.awsEnvSupervisorInternalSpec'.

The actual regression tests for 'Api.Arkham.AwsEnvSupervisor''s credential
acquisition\/release primitives ('ManagedEnvAcquisition', 'AwsEnvAcquisition',
@safe*@ providers, the generic single-thread supervisor protocol, and the
generic demand-driven wrapper) live INSIDE that production module now, not
here -- a MEDIUM finding (\"managed AWS acquisition still forgeable\")
required none of those identifiers, and specifically the full
'Api.Arkham.AwsEnvSupervisor.AwsEnvAcquisition' constructor and
'Api.Arkham.AwsEnvSupervisor.runManagedEnvAcquisition', to ever be
reachable from any module outside that one -- including this one. Moving
their tests to live alongside them (see that module's own
'Api.Arkham.AwsEnvSupervisor.awsEnvSupervisorInternalSpec') keeps every
existing regression case intact while making that constraint provable by
construction: this file (and every other module) has no export list entry
it could ever import to reach any of them.
-}
module Arkham.Api.AwsEnvSupervisorSpec (spec) where

import Api.Arkham.AwsEnvSupervisor (awsEnvSupervisorInternalSpec)
import Arkham.Prelude
import Test.Hspec

spec :: Spec
spec = awsEnvSupervisorInternalSpec
