module Base.Api.Handler.PasswordReset (postApiV1PasswordResetsR, putApiV1PasswordResetR) where

import Base.Api.Types.Account
import Base.Mail (sendPlainTextEmail)
import Crypto.BCrypt
import Data.Text.Encoding qualified as TE
import Data.Time.Clock
import Data.UUID qualified as UUID (toText)
import Import

postApiV1PasswordResetsR :: Handler ()
postApiV1PasswordResetsR = do
  payload <- requireCheckJsonBody :: Handler PasswordResetRequest
  resetExpiresAt <- liftIO $ addUTCTime nominalDay <$> getCurrentTime
  Entity userId _ <- runDB $ getBy404 (UniqueEmail payload.resetEmail)
  token <- runDB $ insert $ PasswordReset userId resetExpiresAt
  sendPasswordResetEmail payload.resetEmail (UUID.toText $ coerce token)
 where
  sendPasswordResetEmail :: Text -> Text -> Handler ()
  sendPasswordResetEmail email token = do
    apiToken <- getsYesod $ appMailtrapApiToken . appSettings
    sendPlainTextEmail
      apiToken
      email
      "Password Reset for Arkham Horror Online"
      "Password Reset"
      [ "You have requested a password reset."
      , ""
      , "To reset your password, click the link below:"
      , ""
      , "https://arkhamhorror.app/#/password-reset/" <> token
      , ""
      , "If you did not request a password reset, you can safely ignore this email."
      ]

putApiV1PasswordResetR :: PasswordResetId -> Handler ()
putApiV1PasswordResetR resetId = do
  payload <- requireCheckJsonBody :: Handler PasswordResetUpdate
  mdigest <-
    liftIO
      $ hashPasswordUsingPolicy
        slowerBcryptHashingPolicy
        (TE.encodeUtf8 payload.resetPassword)
  now <- liftIO getCurrentTime

  runDB $ do
    reset <- get404 resetId
    if now > passwordResetExpiresAt reset
      then delete resetId
      else case mdigest of
        Nothing -> error "could not hash password"
        Just digest -> do
          update (passwordResetUserId reset) [UserPasswordDigest =. TE.decodeUtf8 digest]
          delete resetId
