{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HaskellCurriculum.Yesod.Application where

import HaskellCurriculum.Yesod.Foundation
    ( Route(..), App(..), resourcesApp, AppSettings(..) )
import Yesod ( warp, mkYesodDispatch, liftIO )

import HaskellCurriculum.Yesod.Handler.Home ( getHomeR, getUserR)
import Database.Persist.Postgresql (createPostgresqlPool)
import Control.Monad.Logger
import HaskellCurriculum.Yesod.Orphans ()

-- NOTE(luis) this generates an orphan instance for Dispatch
-- The routes are defined in @Foundation@. This is an alternative
-- to the circular-dependency-prone @mkYesod@ approach
mkYesodDispatch "App" resourcesApp


appMain :: IO ()
appMain = runStderrLoggingT $ do
  pool <- createPostgresqlPool "host=localhost dbname=shoutr" 10
  let settings = AppSettings "so-secret"
  liftIO $ warp 3000 $ App pool settings
