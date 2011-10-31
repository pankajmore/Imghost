{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Vote (postVotedR,postVoteiR) where
import Foundation
import Forms.Vote
import qualified Data.ByteString.Lazy as L
import Helpers.Document
import Control.Applicative
import qualified Data.Text as T
postVotedR :: ImagesId ->Handler RepHtml
postVotedR id = do
    ((dresult, iwidget), denctype) <- runFormPost voteDForm
    case dresult of
        FormSuccess _ -> do 
                        runDB (update id [ImagesVotes -=. 1])
                        redirect RedirectTemporary $ ImageR id
        _ ->  redirect RedirectTemporary $ ImageR id

postVoteiR :: ImagesId ->Handler RepHtml
postVoteiR id = do
    ((iresult, dwidget), ienctype) <- runFormPost voteIForm
    case iresult of
        FormSuccess _ -> do 
                        runDB (update id [ImagesVotes +=. 1])
                        redirect RedirectTemporary $ ImageR id 
        _ ->  redirect RedirectTemporary $ ImageR id
    

