{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Pet where

import Import
import Tool
import Text.Lucius
--import Database.Persist.Postgresql

-- (<$>) = fmap :: Functor f => (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
formPet :: Maybe Pets -> Form Pets
formPet p = renderDivs $ Pets
    <$> areq textField (FieldSettings "Nome: " 
                                      Nothing
                                      (Just "hs12")
                                      Nothing
                                      [("class","myClass")]
                       ) (fmap petsNome p)
    <*> areq intField "Idade: " (fmap petsIdade p)
    <*> areq textField (FieldSettings "Motivo da Visita: " 
                                      Nothing
                                      (Just "hs12")
                                      Nothing
                                      [("class","myClass")]
                       ) (fmap petsMotivoVisita p)

auxPetR :: Route App -> Maybe Pets -> Handler Html
auxPetR rt petz = do
    sess <- lookupSession "_EMAIL"
    case sess of 
        Nothing -> redirect ForbiddenR
        Just _ -> do
            (widget,_) <- generateFormPost (formPet petz)
            defaultLayout $ do 
                sess <- lookupSession "_EMAIL"
                valid <- lookupSession "_ID"
                toWidgetHead $(luciusFile  "templates/header.lucius")
                $(whamletFile "templates/header.hamlet")
                [whamlet|
                    <h1>
                        CADASTRO DE PET
                    
                    <form action=@{rt} method=post>
                        ^{widget}
                        <input type="submit" value="Cadastrar">
                |]
    
getPetR :: Handler Html
getPetR = auxPetR PetR Nothing
    
postPetR :: Handler Html
postPetR = do
    sess <- lookupSession "_EMAIL"
    case sess of 
        Nothing -> redirect ForbiddenR
        Just _ -> do
            ((resp,_),_) <- runFormPost (formPet Nothing)
            case resp of 
                FormSuccess petz -> do 
                    pid <- runDB $ insert petz
                    redirect (DescPetR pid)
                _ -> redirect HomeR

-- SELECT * from petz where id = pid 
getDescPetR :: PetsId -> Handler Html
getDescPetR pid = do  
    sess <- lookupSession "_EMAIL"
    case sess of 
        Nothing -> redirect ForbiddenR
        Just _ -> do
            petz <- runDB $ get404 pid
            (widget,_) <- generateFormPost formDesc
            defaultLayout $ do 
                sess <- lookupSession "_EMAIL"
                valid <- lookupSession "_ID"
                toWidgetHead $(luciusFile  "templates/header.lucius")
                $(whamletFile "templates/header.hamlet")
                [whamlet|
                <h1>
                    Nome: #{petsNome petz}
                
                <h2>
                    Idade: #{petsIdade petz}

                <h3>
                    Idade: #{petsMotivoVisita petz}
                
                <form action=@{ConsultarR pid} method=post>
                    ^{widget}
                    <input type="submit" value="Enviar">
                |]

getListPetR :: Handler Html
getListPetR = do 
    sess <- lookupSession "_EMAIL"
    case sess of 
        Nothing -> redirect ForbiddenR
        Just _ -> do 
            pets <- runDB $ selectList [] [Desc PetsIdade]
            defaultLayout $ do 
                sess <- lookupSession "_EMAIL"
                valid <- lookupSession "_ID"
                toWidgetHead $(luciusFile  "templates/header.lucius")
                $(whamletFile "templates/header.hamlet")
                [whamlet|
                    <table>
                        <thead>
                            <tr>
                                <th> 
                                    Nome
                                
                                <th>
                                    Pet
                                
                                <th>
                                    Motivo da Consulta
                        <tbody>
                            $forall Entity pid p <- pets
                                <tr>
                                    <td>
                                        <a href=@{DescPetR pid}>
                                        #{petsNome p}
                                    
                                    <td>
                                        #{petsIdade p}

                                    <td>
                                        #{petsMotivoVisita p}
                                    
                                    <th>
                                        <a href=@{UpdPetR pid}>
                                            Editar
                                    $if null valid
                                        <th>
                                            
                                    $else
                                        <th>
                                            <form action=@{DelPetR pid} method=post>
                                                <input type="submit" value="ASD">
            |]

getUpdPetR :: PetsId -> Handler Html
getUpdPetR pid = do 
    sess <- lookupSession "_EMAIL"
    case sess of 
        Nothing -> redirect ForbiddenR
        Just _ -> do
            antigo <- runDB $ get404 pid
            auxPetR (UpdPetR pid) (Just antigo)    
    
-- UPDATE petz WHERE id = pid SET ...
postUpdPetR :: PetsId -> Handler Html
postUpdPetR pid = do 
    sess <- lookupSession "_EMAIL"
    defaultLayout $ do 
        sess <- lookupSession "_EMAIL"
        valid <- lookupSession "_ID"
        toWidgetHead $(luciusFile  "templates/header.lucius")
        $(whamletFile "templates/header.hamlet")
    case sess of 
        Nothing -> redirect ForbiddenR
        Just _ -> do
            ((resp,_),_) <- runFormPost (formPet Nothing)
            case resp of 
                FormSuccess novo -> do
                    runDB $ replace pid novo
                    redirect (DescPetR pid) 
                _ -> redirect HomeR

postDelPetR :: PetsId -> Handler Html
postDelPetR pid = do 
    sess <- lookupSession "_EMAIL"
    case sess of 
        Nothing -> redirect ForbiddenR
        Just _ -> do
            _ <- runDB $ get404 pid 
            runDB $ delete pid 
            redirect ListPetR