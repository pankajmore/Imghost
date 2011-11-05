{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Models where

import Yesod 
import Database.Persist.Sqlite
import Data.Text(Text)
import Data.Time

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
SqlImage
    name Text
    tag Text
    owner UserId Maybe
    caption Text
    votes Int
    hits Int 
    created UTCTime
    UniqueName name

User
    name  Text Maybe Update
    email Text Maybe Update
    admin Bool default=false Eq Update

Votes
    userId UserId
    imageId SqlImageId
    value Int
    UniqueVote userId imageId

Ident
    ident Text   Asc
    user  UserId Eq
    UniqueIdent ident
|]

data Image = Image 
    { name :: Text
    , tag :: Text
    , owner :: Maybe UserId
    , caption :: Text
    , votes :: Int
    , hits :: Int
    , created :: UTCTime
    } deriving Show 



data Img = Img
    { img :: FileInfo
    , tags :: Text
    }deriving Show

