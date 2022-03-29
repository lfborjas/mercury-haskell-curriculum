-- | From: https://github.com/MercuryTechnologies/haskell-curriculum/tree/49dea88cac791f514e3f60c5472d0c0481dc4418/tracks/intermediate/associated-type-families
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass, DerivingStrategies, DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module HaskellCurriculum.Intermediate.TypeFamilies where


import Network.HTTP.Simple
import Data.Aeson
import GHC.Generics
import Data.Time
import qualified Data.ByteString.Char8 as BS8

class ToQuery a where
  toQuery :: a -> Query

data ClueOptions = ClueOptions
  { value :: Maybe Int
  , category :: Maybe Int
  , min_date :: Maybe UTCTime
  , max_date :: Maybe UTCTime
  , offset :: Maybe Int
  }

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


apiGet :: (FromJSON a, ToQuery query) => String -> query -> IO a
apiGet path query =
  getResponseBody <$> httpJSON
        (setRequestQueryString (toQuery query)
         $ parseRequest_
         $ "https://jservice.io/api/"
         <> path
        )

tfMain :: IO ()
-- NOTE: very nice application of type applications: we can tell it
-- what to parse as!
-- NOTE: needed a type signature for @ClueOptions@, due to using DuplicateRecordFields extension
tfMain = apiGet @[Clue] "clues" (defaultClueOptions{value = Just 500} :: ClueOptions) >>= print
