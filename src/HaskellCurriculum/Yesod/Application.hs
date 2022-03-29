{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HaskellCurriculum.Yesod.Application where

import HaskellCurriculum.Yesod.Foundation
    ( Route(HomeR), App(..), resourcesApp )
import Yesod ( warp, mkYesodDispatch )

import HaskellCurriculum.Yesod.Handler.Home ( getHomeR )

-- NOTE(luis) this generates an orphan instance
mkYesodDispatch "App" resourcesApp

appMain :: IO ()
appMain = warp 3000 App
