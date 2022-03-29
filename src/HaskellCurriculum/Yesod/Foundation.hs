-- |
{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies #-}

module HaskellCurriculum.Yesod.Foundation where

import Yesod

data App = App

instance Yesod App

-- NOTE(luis) from the tutorial: mkYesodData doesn't provide
-- an instance of YesodDispatch
mkYesodData "App" [parseRoutes|
               / HomeR GET
                |]

