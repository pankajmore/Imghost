{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Models where

import Yesod 
import Database.Persist.Sqlite
import Data.Text(Text)
import Data.Time
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Images
    imageName String
    imageTag String
    votes Int
    created UTCTime
    ImageName imageName

User
    ident Text
    password Text Maybe
    UniqueUser ident

Ident
    ident Text   Asc
    user  UserId Eq
    UniqueIdent ident
|]
data Img = Img
    { img :: FileInfo 
    , tags :: Text
    }deriving Show

