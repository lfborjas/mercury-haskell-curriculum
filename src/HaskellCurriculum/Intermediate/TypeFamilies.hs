-- | From: https://github.com/MercuryTechnologies/haskell-curriculum/tree/49dea88cac791f514e3f60c5472d0c0481dc4418/tracks/intermediate/associated-type-families
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass, DerivingStrategies, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts #-}
module HaskellCurriculum.Intermediate.TypeFamilies where


import Network.HTTP.Simple
    ( parseRequest_,
      getResponseBody,
      httpJSON,
      setRequestQueryString,
      Query,
      QueryItem )
import Data.Aeson
import GHC.Generics
import Data.Time
import qualified Data.ByteString.Char8 as BS8
import Data.Proxy

class ToQuery a where
  toQuery :: a -> Query

data ClueOptions = ClueOptions
  { value :: Maybe Int
  , category :: Maybe Int
  , min_date :: Maybe UTCTime
  , max_date :: Maybe UTCTime
  , offset :: Maybe Int
  }

-- Associated type family
-- https://github.com/MercuryTechnologies/haskell-curriculum/tree/49dea88cac791f514e3f60c5472d0c0481dc4418/tracks/intermediate/associated-type-families#associated-type-families-to-the-rescue
-- To ensure that two types are related
class Endpoint a where
  type ApiPayload a
  type ApiResponse a
  path :: proxy a -> String
  payload :: a -> ApiPayload a

q :: Show a => BS8.ByteString -> Maybe a -> QueryItem
q label val = (label, BS8.pack . show <$> val)

instance ToQuery ClueOptions where
  toQuery ClueOptions{..} =
    [ q "value" value
    , q "category" category
    , q "min_date" min_date
    , q "max_date" max_date
    , q "offset" offset
    ]

defaultClueOptions :: ClueOptions
defaultClueOptions = ClueOptions Nothing Nothing Nothing Nothing Nothing

data Clue = Clue
  { id :: Int
  , question :: String
  , value :: Int
  }
  -- NOTE(luis) the tutorial was missing the Show instance
  deriving stock (Show, Generic)
  -- NOTE(luis) the tutorial actually spells out the instance
  -- but it seems like the generic one will do?
  deriving anyclass FromJSON

newtype CluesEndpoint = CluesEndpoint ClueOptions

instance Endpoint CluesEndpoint where
  type ApiPayload CluesEndpoint = ClueOptions
  type ApiResponse CluesEndpoint = [Clue]
  path _ = "clues"
  payload (CluesEndpoint options) = options

--apiGet :: (FromJSON a, ToQuery query) => String -> query -> IO a
apiGet
  :: forall a
   . (Endpoint a, FromJSON (ApiResponse a), ToQuery (ApiPayload a))
   => a
   -> IO (ApiResponse a)
apiGet endpoint =
  getResponseBody <$> httpJSON
        (setRequestQueryString (toQuery $ payload endpoint)
         $ parseRequest_
         $ "https://jservice.io/api/"
        -- NOTE: this is Data.Proxy.Proxy, /not/ the one in HTTP!!
         <> path (Proxy @a)
        )

tfMain :: IO ()
-- NOTE: very nice application of type applications: we can tell it
-- what to parse as!
-- NOTE: needed a type signature for @ClueOptions@, due to using DuplicateRecordFields extension
tfMain = apiGet (CluesEndpoint $ (defaultClueOptions{value = Just 500} :: ClueOptions)) >>= print
