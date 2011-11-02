{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,TemplateHaskell, OverloadedStrings #-}
module Handler.Vote ( postVotedR
                    , postVoteiR
                    , getVoteiR
                    , getVotedR
                    ) where
import Foundation
import Forms.Image
import qualified Data.ByteString.Lazy as L
import Helpers.Document
import Control.Applicative
import qualified Data.Text as T
postVotedR :: ImagesId ->Handler RepHtml
postVotedR id = do
    ((dresult, dwidget), denctype) <- runFormPost (imageForm images_thumbsdown_jpg) 
    case dresult of
        FormSuccess _ -> do
                        currentUserid <- requireAuthId
                        alreadyInVotes <- runDB (getBy $ UniqueVote currentUserid id)
                        case alreadyInVotes of 
                            Nothing -> do runDB (insert $ Votes currentUserid id (-1))
                                          runDB (update id [ImagesVotes -=. 1])
                            Just (qid , val) -> do
                                                case votesValue val of
                                                    0 ->  do
                                                        runDB (update qid [ VotesValue =. (-1)])
                                                        runDB (update id [ImagesVotes -=. 1])
                                                    1 -> do
                                                        runDB (update qid [ VotesValue =. 0])
                                                        runDB (update id [ImagesVotes -=. 1])
                                                    (-1) -> return ()
                        redirect RedirectTemporary $ ImageR id
        _ ->  redirect RedirectTemporary $ ImageR id

postVoteiR :: ImagesId ->Handler RepHtml
postVoteiR id = do
    ((iresult, iwidget), ienctype) <- runFormPost (imageForm images_thumbsup_jpg)
    case iresult of
        FormSuccess _ -> do 
                        currentUserid <- requireAuthId
                        alreadyInVotes <- runDB (getBy $ UniqueVote currentUserid id)
                        case alreadyInVotes of 
                            Nothing -> do runDB (insert $ Votes currentUserid id 1)
                                          runDB (update id [ImagesVotes +=. 1])
                            Just (qid , val) -> do 
                                                case votesValue val of
                                                    0 ->  do
                                                        runDB (update qid [ VotesValue =. 1])
                                                        runDB (update id [ImagesVotes +=. 1])
                                                    (-1) -> do
                                                        runDB (update qid [ VotesValue =. 0])
                                                        runDB (update id [ImagesVotes +=. 1])
                                                    1 -> return ()

                        redirect RedirectTemporary $ ImageR id 
        _ ->  redirect RedirectTemporary $ ImageR id
    
getVoteiR = postVoteiR
getVotedR = postVotedR
