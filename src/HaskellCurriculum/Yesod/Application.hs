{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HaskellCurriculum.Yesod.Application where

import HaskellCurriculum.Yesod.Foundation
    ( Route(HomeR), App(..), resourcesApp )
import Yesod ( warp, mkYesodDispatch, liftIO )

import HaskellCurriculum.Yesod.Handler.Home ( getHomeR )
import Database.Persist.Postgresql (createPostgresqlPool)
import Control.Monad.Logger
import HaskellCurriculum.Yesod.Orphans ()

-- NOTE(luis) this generates an orphan instance
mkYesodDispatch "App" resourcesApp

appMain :: IO ()
appMain = runStderrLoggingT $ do
  pool <- createPostgresqlPool "host=localhost dbname=shoutr" 10
  liftIO $ warp 3000 $ App pool
