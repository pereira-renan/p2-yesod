{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Usuario where

import Import
import Tool
import Text.Lucius

formUsu :: Form (Usuario, Text)
formUsu = renderBootstrap $ (,)
    <$> (Usuario 
        <$> areq textField "Nome:" Nothing
        <*> areq emailField "E-mail:" Nothing
        <*> areq passwordField "Senha:" Nothing)
    <*> areq passwordField "Digite Novamente:" Nothing

getCadastrarR :: Handler Html
getCadastrarR = do 
    (widget,_) <- generateFormPost formUsu
    sess <- lookupSession "_EMAIL"
    valid <- lookupSession "_ID"
    case sess of 
        Nothing -> do
            msg <- getMessage
            defaultLayout $ do 
                setTitle "Cadastro"
                toWidgetHead $(luciusFile  "templates/header.lucius")
                $(whamletFile "templates/header.hamlet")
                toWidgetHead $(luciusFile  "templates/form.lucius")
                geraForm CadastrarR "CADASTRO" "Cadastrar" msg widget
        Just _ -> redirect HomeR

postCadastrarR :: Handler Html
postCadastrarR = do 
    ((result,_),_) <- runFormPost formUsu
    case result of 
        FormSuccess (usuario,veri) -> do 
            if (usuarioSenha usuario == veri) then do 
                runDB $ insert400 usuario 
                setMessage [shamlet|
                    <div class="success">
                        Usuário Cadastrado!
                |]
                redirect CadastrarR
            else do 
                setMessage [shamlet|
                    <div class="error">
                        Email ou Senha Incorretos!
                |]
                redirect CadastrarR
        _ -> redirect HomeR
    
    
