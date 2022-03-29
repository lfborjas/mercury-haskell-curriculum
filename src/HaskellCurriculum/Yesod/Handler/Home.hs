-- |

{-# LANGUAGE DeriveAnyClass #-}
module HaskellCurriculum.Yesod.Handler.Home where
import HaskellCurriculum.Yesod.Foundation (Handler)
import Yesod
import Data.Text (Text)
import HaskellCurriculum.Yesod.Model.User
import GHC.Generics
import Prelude hiding (id)
import Data.Aeson

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
