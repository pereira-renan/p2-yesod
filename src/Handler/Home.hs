{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
import Database.Persist.Postgresql

--         <img src=@{StaticR img_produto_jpg}>
getHomeR :: Handler Html
getHomeR = defaultLayout $ do 
    addStylesheet (StaticR css_bootstrap_css)
    sess <- lookupSession "_EMAIL"
    valid <- lookupSession "_ID"
    [whamlet|
        <h1>
            PETPARTY - SISTEMA DE AGENDAMENTOS DE CONSULTA
        <ul>
            <li> 
                <a href=@{PetR}>
                    Cadastrar Novos Pets

            <li>
                <a href=@{ListPetR}>
                    Listar Pets Existentes
                    
            <li>
                <a href=@{ListVetR}>
                    Listar Veterinarios Existentes

            $if null valid
                <li>
                    Usuário nível padrão.
            $else
                <li>
                    Usuário nível admin.

            $maybe email <- sess
                    <div>
                        #{email}
                        <form method=post action=@{SairR}>
                            <input type="submit" value="Sair">
            $nothing
                <li>
                    <a href=@{CadastrarR}>
                        Cadastrar Usuário
                    
                <li>
                    <a href=@{EntrarR}>
                        Login
    |]
