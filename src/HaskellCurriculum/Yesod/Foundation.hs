-- |
{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, ViewPatterns #-}
module HaskellCurriculum.Yesod.Foundation where

import Yesod
import Data.Pool
import Database.Persist.Postgresql
import Data.Text (Text)
import HaskellCurriculum.Yesod.Model.User

-- NOTE(luis) from the exercises
-- https://github.com/MercuryTechnologies/haskell-curriculum/blob/49dea88cac791f514e3f60c5472d0c0481dc4418/tracks/web-development/yesod/Intro.md#exercises
-- Note that "static" settings coming from env/config files are recommended
-- as a separate data type, while other more dynamic (pools, log functions) values
-- are direct members of @App@
-- real-life ex: https://github.com/MercuryTechnologies/mercury-web-backend/blob/f5896eae8576a5145af12edf7be8facd73a44b12/src/Settings.hs
data AppSettings = AppSettings
  { jwtSecret :: Text }

data App = App
  { appConnectionPool :: Pool SqlBackend
  , appSettings :: AppSettings
  }

-- From: https://github.com/MercuryTechnologies/haskell-curriculum/blob/49dea88cac791f514e3f60c5472d0c0481dc4418/tracks/web-development/yesod/Intro.md#handler
-- | Use this instance if you only need access to the @App@ context,
-- and not the whole of Yesod
class Monad m => HasApp m where
  getApp :: m App
  -- NOTE(luis) from the exercises
  --getsApp :: (m App -> a) -> m a

-- from our real codebase:
-- https://github.com/MercuryTechnologies/mercury-web-backend/blob/f5896eae8576a5145af12edf7be8facd73a44b12/src/App.hs#L230-L231
getsApp :: (HasApp m) => (App -> a) -> m a
getsApp = flip fmap getApp

instance HasApp (HandlerFor App) where
  getApp = getYesod

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    pool <- getsYesod appConnectionPool
    runSqlPool action pool

-- NOTE(luis) from the exercises
getSettings :: HasApp m => m AppSettings
getSettings = getsApp appSettings

-- NOTE(luis) from the exercises
getJWT :: HasApp m => m Text
getJWT = getsApp (jwtSecret . appSettings)

-- NOTE(luis) from the tutorial: mkYesodData doesn't provide
-- an instance of YesodDispatch
-- Also see the Intro for more interesting Yesod arcana:
-- https://github.com/MercuryTechnologies/haskell-curriculum/blob/49dea88cac791f514e3f60c5472d0c0481dc4418/tracks/web-development/yesod/Intro.md
-- (expanding the splice in-place creates a whole bunch of interesting code, dissimilar to what `mkYesod` would generate:
-- https://www.yesodweb.com/book/basics#basics_routing)
-- We could /also/ call @parseRoutesFile FilePath@ in a splice to instead read
-- the routes from a file.
-- NOTE(luis) adding the users route made ViewPatterns necessary??
mkYesodData "App" [parseRoutes|
               / HomeR GET
               /users/#UserId UserR GET
                |]

-- From:
-- https://www.yesodweb.com/book-1.6/authentication-and-authorization
-- and: https://www.yesodweb.com/book-1.6/yesod-typeclass
-- note that for /authentication/ there's also @YesodAuth@
-- where all the auth bits are supposed to happen.
instance Yesod App where
  isAuthorized (UserR u) _ = isRequestForSelf u
  isAuthorized _ _ = pure Authorized

  -- From the tutorial:
-- https://github.com/MercuryTechnologies/haskell-curriculum/blob/49dea88cac791f514e3f60c5472d0c0481dc4418/tracks/web-development/yesod/Intro.md#the-user

sessionUserId :: MonadHandler m => m (Maybe UserId)
sessionUserId = (>>= fromPathPiece) <$> lookupBearerAuth

-- NOTE(luis) the tutorial also has some incomplete examples of
-- how to get this from alternative sources:
-- https://github.com/MercuryTechnologies/haskell-curriculum/blob/49dea88cac791f514e3f60c5472d0c0481dc4418/tracks/web-development/yesod/Intro.md#the-user
currentUserId :: Handler (Maybe UserId)
currentUserId = sessionUserId

requireUserId :: Handler UserId
requireUserId = do
  maybeUserId <- currentUserId
  case maybeUserId of
    Just uid -> pure uid
    -- TODO need to require http types for status401 to be available
    Nothing -> notAuthenticated --sendStatusJSON status401 mempty

-- | Meant to be used in @isAuthorized@ for the Yesod instance.
isRequestForSelf :: UserId -> Handler AuthResult
isRequestForSelf requestedUid =
  requireUserId >>= (\uid -> pure $ if uid == requestedUid then Authorized else Unauthorized "Not allowed")
