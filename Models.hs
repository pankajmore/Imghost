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

Ident
    ident Text   Asc
    user  UserId Eq
    UniqueIdent ident
|]
data JsonImage = JsonImage 
    { name :: String
    , caption :: String
    , tag :: String 
    } deriving Show 

instance ToJSON JsonImage where 
    toJSON image = object [T.pack "src" .= (name image),T.pack "name" .= (caption image) ,T.pack "tag" .= (tag image)]

data Img = Img
    { img :: FileInfo
    , tags :: Text
    }deriving Show

