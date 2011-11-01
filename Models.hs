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
    caption String
    votes Int
    created UTCTime
    ImageName imageName
|]
data JsonImage = JsonImage 
    { name :: String
    , caption :: String
    , tag :: String 
    } deriving Show 

instance ToJSON JsonImage where 
    toJSON image = object [T.pack "name" .= (name image),T.pack "caption" .= (caption image) ,T.pack "tag" .= (tag image)]

data Img = Img
    { img :: FileInfo
    , tags :: Text
    }deriving Show

