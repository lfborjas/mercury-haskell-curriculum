{-#LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- |

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HaskellCurriculum.Yesod.Model.User where

-- NOTE(luis) the tutorial says to use persistent... but I also had to add persistent-template
import Database.Persist.TH
import Data.Text

-- NOTE(luis) I /think/ the tutorial has the table wrong here, too (singular vs. plural, as eventually created)
share [mkPersist sqlSettings] [persistLowerCase|
User sql=users
  email Text
  username Text
  UniqueEmail email
  UniqueUsername email
|]
