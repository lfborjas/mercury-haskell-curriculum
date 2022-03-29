-- |

{-# LANGUAGE DeriveAnyClass #-}
module HaskellCurriculum.Yesod.Handler.Home where
import HaskellCurriculum.Yesod.Foundation (Handler)
import Yesod
    ( typeJson,
      ToJSON,
      ToTypedContent(..),
      TypedContent(TypedContent),
      ToContent(..),
      PathPiece(fromPathPiece),
      PersistEntity(Key),
      selectList,
      lookupBearerAuth,
      Entity(Entity),
      MonadHandler,
      YesodPersist(runDB), sendStatusJSON, notAuthenticated, permissionDenied, selectFirst, notFound )
import Data.Text (Text)
import HaskellCurriculum.Yesod.Model.User ( User(..), UserId )
import GHC.Generics ( Generic )
import Prelude hiding (id)
import Data.Aeson ( encode )
import Database.Persist

data ApiUser = ApiUser
  { id :: Key User
  , email :: Text
  , username :: Text
  }
  deriving stock Generic
  deriving anyclass ToJSON

instance ToTypedContent ApiUser where
  toTypedContent = TypedContent typeJson . toContent

instance ToContent ApiUser where
  toContent = toContent . encode

toApiUser :: Entity User -> ApiUser
toApiUser (Entity userId User {userEmail, userUsername}) =
  ApiUser {id = userId, email = userEmail, username = userUsername}

getHomeR :: Handler [ApiUser]
getHomeR  = runDB $ map toApiUser <$> selectList [] []

-- e.g. curl -H "Authorization: Bearer 1"  -v localhost:3000/users/1
getUserR :: UserId -> Handler ApiUser
getUserR uid = do
  requestingUid <- requireUserId
  if requestingUid == uid
    then do
      fromDb <- runDB $ get uid
      case fromDb of
        Nothing -> notFound
        Just u  -> pure $ toApiUser $ Entity uid u
    else permissionDenied "Nope"


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
