{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies #-}

module HaskellCurriculum.Yesod.Application where

import Yesod

data App = App

instance Yesod App

mkYesod "App" [parseRoutes|
               / HomeR GET
                |]

getHomeR :: Handler ()
getHomeR  = pure ()

appMain :: IO ()
appMain = warp 3000 App
