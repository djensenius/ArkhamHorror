{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Auth.JWTSpec (spec) where

import Auth.JWT (jsonToToken, tokenToJson)
import Control.Lens ((?~))
import Crypto.JWT hiding (jwk)
import Data.Aeson (ToJSON (..), Value (..), toJSON)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Relude
import Test.Hspec

-- HS256 requires a minimum key size of 256 bits (32 bytes).
testSecret :: Text
testSecret = "test-jwt-secret-for-unit-tests-x"  -- 32 chars

testPayload :: Value
testPayload = String "user-42"

-- Mirror the production Super subtype to avoid the deprecated unregisteredClaims
-- lens.  Only used in the test-fixture helper below.
data ExpiredClaims = ExpiredClaims {ecClaims :: ClaimsSet, ecJwt :: Value}

instance HasClaimsSet ExpiredClaims where
  claimsSet f s = fmap (\a' -> s {ecClaims = a'}) (f (ecClaims s))

instance ToJSON ExpiredClaims where
  toJSON ec = ins "jwt" (ecJwt ec) (toJSON (ecClaims ec))
   where
    ins k v (Object o) = Object $ KM.insert k (toJSON v) o
    ins _ _ a = a

-- | Build a JWT signed with the given secret but with an exp claim set one
-- hour in the past. Uses the same HS256 key construction as production code.
makeExpiredToken :: Text -> Value -> IO Text
makeExpiredToken secret payload = do
  now <- getCurrentTime
  let pastExp = addUTCTime (-3600) now
      key = fromOctets (encodeUtf8 @Text @BSL.ByteString secret)
      claims =
        ExpiredClaims
          { ecClaims =
              emptyClaimsSet
                & (claimExp ?~ NumericDate pastExp)
                & (claimIss ?~ "arkham")
          , ecJwt = payload
          }
  res <- runJOSE @JWTError $ signJWT key (newJWSHeaderProtected HS256) claims
  case res of
    Left err -> error $ "makeExpiredToken: test-setup failure: " <> show err
    Right tkn -> pure $ TL.toStrict $ TL.decodeUtf8 $ encodeCompact tkn

spec :: Spec
spec = do
  describe "tokenToJson" do
    it "returns Just the payload for a valid token" do
      token <- jsonToToken testSecret testPayload
      result <- tokenToJson testSecret token
      result `shouldBe` Just testPayload

    it "returns Nothing for a wrong signing key (signature mismatch)" do
      token <- jsonToToken testSecret testPayload
      result <- tokenToJson "different-secret-also-32-bytes-!" token
      result `shouldBe` Nothing

    it "returns Nothing for a structurally malformed token" do
      result <- tokenToJson testSecret "not.a.valid.jwt"
      result `shouldBe` Nothing

    it "returns Nothing for an empty string" do
      result <- tokenToJson testSecret ""
      result `shouldBe` Nothing

    it "returns Nothing for a signature-tampered token" do
      token <- jsonToToken testSecret testPayload
      -- Replace the first character of the signature segment with a different one.
      let parts = T.splitOn "." token
          tampered = case parts of
            [h, p, sig] ->
              T.intercalate "."
                [ h
                , p
                , T.cons (if T.head sig == 'A' then 'B' else 'A') (T.tail sig)
                ]
            _ -> token <> "x"
      result <- tokenToJson testSecret tampered
      result `shouldBe` Nothing

    it "returns Nothing for an expired token" do
      token <- makeExpiredToken testSecret testPayload
      result <- tokenToJson testSecret token
      result `shouldBe` Nothing
