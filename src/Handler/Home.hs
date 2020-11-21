{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
import Text.Hamlet
import Text.Lucius
import Database.Persist.Postgresql

--         <img src=@{StaticR img_produto_jpg}>
getHomeR :: Handler Html
getHomeR = defaultLayout $ do 
    sess <- lookupSession "_EMAIL"
    valid <- lookupSession "_ID"
    toWidgetHead $(luciusFile  "templates/header.lucius")
    $(whamletFile "templates/header.hamlet")
    toWidgetHead $(luciusFile  "templates/home.lucius")
    $(whamletFile "templates/home.hamlet")