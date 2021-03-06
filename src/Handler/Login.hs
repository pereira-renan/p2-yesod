{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Import
import Tool
import Text.Lucius

formLogin :: Form (Text, Text)
formLogin = renderBootstrap $ (,)
    <$>  areq emailField "E-mail: " Nothing
    <*>  areq passwordField "Senha: " Nothing
    
getEntrarR :: Handler Html
getEntrarR = do 
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do 
        sess <- lookupSession "_EMAIL"
        valid <- lookupSession "_ID"
        setTitle "Login"
        toWidgetHead $(luciusFile  "templates/header.lucius")
        $(whamletFile "templates/header.hamlet")
        toWidgetHead $(luciusFile  "templates/form.lucius")
        geraForm EntrarR "Login" "Enviar" msg widget
        [whamlet|
            <form class="formCampos" action=@{CadastrarR} method=get>
                <input type="submit" value="Cadastro"> 
        |]

postEntrarR :: Handler Html
postEntrarR = do 
    ((result,_),_) <- runFormPost formLogin
    case result of 
        FormSuccess ("admin@admin.com", "123") -> do
            setSession "_EMAIL" "admin@admin.com"
            setSession "_ID" "admin"
            redirect HomeR
        FormSuccess (email,senha) -> do 
           -- select * from usuario where email=digitado.email
           usuario <- runDB $ getBy (UniqueEmail email)
           case usuario of 
                Nothing -> do 
                    setMessage [shamlet|
                        <div class="error">
                            Email Não Encontrado!
                    |]
                    redirect EntrarR
                Just (Entity _ usu) -> do 
                    if (usuarioSenha usu == senha) then do
                        setSession "_EMAIL" (usuarioEmail usu)
                        redirect HomeR
                    else do 
                        setMessage [shamlet|
                            <div class="error">
                                Senha Inserida Incorreta!
                        |]
                        redirect EntrarR 
        _ -> do
            setSession "_ID" ""
            redirect HomeR
        
postSairR :: Handler Html 
postSairR = do 
    deleteSession "_EMAIL"
    deleteSession "_ID"
    redirect HomeR

getAdminR :: Handler Html
getAdminR = defaultLayout $ do 
    sess <- lookupSession "_EMAIL"
    valid <- lookupSession "_ID"
    setTitle "Admin"
    toWidgetHead $(luciusFile  "templates/header.lucius")
    $(whamletFile "templates/header.hamlet")
    [whamlet|
            <h1>
                BEM-VINDO ADMIN
    |]
