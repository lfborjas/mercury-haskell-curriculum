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
      YesodPersist(runDB), sendStatusJSON, notAuthenticated, permissionDenied, selectFirst, notFound, AuthResult (Authorized, Unauthorized) )
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
  -- NOTE: authorization is handled in the Yesod instance.
  fromDb <- runDB $ get uid
  case fromDb of
    Nothing -> notFound
    Just u  -> pure $ toApiUser $ Entity uid u
