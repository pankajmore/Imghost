{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Models where

import Yesod 
import Data.Text(Text)
import Data.Time
import Control.Applicative
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
User
    name  Text Maybe Update
    email Text Maybe Update
    admin Bool default=false Eq Update

SqlImage
    name Text
    tags Text
    owner UserId Maybe
    caption Text
    votes Int
    hits Int 
    created UTCTime
    UniqueName name

Tag 
    imageId SqlImageId 
    tag Text
    created UTCTime   -- To sort on the created time descending order
    UniqueTag imageId tag 

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
    , tags :: Text
    , owner :: Maybe UserId
    , caption :: Text
    , votes :: Int
    , hits :: Int
    , created :: UTCTime
    } deriving Show 



data Img = Img
    { iimg :: FileInfo
    , itags :: [Text]
    }deriving Show







