{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Forbidden where

import Import
import Database.Persist.Postgresql

--         <img src=@{StaticR img_produto_jpg}>
getForbiddenR :: Handler Html
getForbiddenR = defaultLayout $ do 
    addStylesheet (StaticR css_bootstrap_css)
    [whamlet|
        <h1>
            Você não tem permissão para acessar a página solicitada.
    |]
