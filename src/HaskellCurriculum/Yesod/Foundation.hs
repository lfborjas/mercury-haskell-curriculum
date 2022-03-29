-- |
{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies #-}

module HaskellCurriculum.Yesod.Foundation where

import Yesod
import Data.Pool
import Database.Persist.Postgresql

data App = App
  { appConnectionPool :: Pool SqlBackend }

instance Yesod App

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    --app <- getYesod
    pool <- getsYesod appConnectionPool
    runSqlPool action pool

-- NOTE(luis) from the tutorial: mkYesodData doesn't provide
-- an instance of YesodDispatch
mkYesodData "App" [parseRoutes|
               / HomeR GET
                |]

