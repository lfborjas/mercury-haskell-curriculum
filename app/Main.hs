module Main where

--import HaskellCurriculum (run)
import HaskellCurriculum.Yesod.Application (appMain)
import HaskellCurriculum.Intermediate.TypeFamilies (tfMain)

main :: IO ()
main = tfMain
