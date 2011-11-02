{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Models where

import Yesod 
import Database.Persist.Sqlite
import Data.Text(Text)
import qualified Data.Text as T 
import Data.Time
import Data.Aeson
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Images
    imageName String
    imageTag String
    owner UserId Maybe
    caption String
    votes Int
    created UTCTime
    ImageName imageName

User
    name  Text Maybe Update
    email Text Maybe Update
    admin Bool default=false Eq Update

Votes
    userid UserId
    imageid ImagesId
    value Int
    UniqueVote userid imageid

Ident
    ident Text   Asc
    user  UserId Eq
    UniqueIdent ident
|]
data JsonImage = JsonImage 
    { name :: String
    , caption :: String
    , tag :: String
    , lnk :: Text
    } deriving Show 

instance ToJSON JsonImage where 
    toJSON image = object ["link" .= (lnk image),"src" .= (name image),"name" .= (caption image) ,"tag" .= (tag image)]

data Img = Img
    { img :: FileInfo
    , tags :: Text
    }deriving Show

