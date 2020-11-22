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
formPet p = renderBootstrap $ Pets
    <$> areq textField (FieldSettings "Nome" 
                                      Nothing
                                      (Just "hs12")
                                      Nothing
                                      [("class","petNome")]
                       ) (fmap petsNome p)
    <*> areq intField (FieldSettings "Idade" 
                                      Nothing
                                      (Just "hs12")
                                      Nothing
                                      [("class","petIdade")]
                      )(fmap petsIdade p)
    <*> areq textareaField (FieldSettings "Motivo da Visita" 
                                      Nothing
                                      (Just "hs12")
                                      Nothing
                                      [("class","petMotivo")]
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
                toWidgetHead $(luciusFile  "templates/form.lucius")
                [whamlet|
                    <div class="form">
                        <h1>
                            AGENDAMENTO
                        <br>
                        <br>
                        <h3 class="intro">
                            Realize aqui seu agendamento e compareça na data do veterinário de sua preferência
                        
                        <form action=@{rt} method=post>
                            ^{widget}
                            <br>
                                <input type="submit" value="Confirmar Agendamento">
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
                    redirect (ConfirmaPetR pid)
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
                toWidgetHead $(luciusFile  "templates/form.lucius")
                [whamlet|
                    <div class="form">
                        <h1>
                            ATENDIMENTO
                        <br>
                        <br>
                        <h2>
                            Nome<br>
                        <h3>
                            #{petsNome petz}
                        <h2>
                            Idade<br>
                        <h3>
                            #{petsIdade petz}
                        <h2>
                            Motivo da Visita<br>
                        <h3>
                            #{petsMotivoVisita petz}
                        <br>                            
                        <form action=@{ConsultarR pid} method=post>
                            ^{widget}
                            <br>
                                <input type="submit" value="Finalizar Consulta">
                |]

getConfirmaPetR :: PetsId -> Handler Html
getConfirmaPetR pid = do  
    sess <- lookupSession "_EMAIL"
    case sess of 
        Nothing -> redirect ForbiddenR
        Just _ -> do
            petz <- runDB $ get404 pid
            defaultLayout $ do 
                sess <- lookupSession "_EMAIL"
                valid <- lookupSession "_ID"
                toWidgetHead $(luciusFile  "templates/header.lucius")
                $(whamletFile "templates/header.hamlet")
                toWidgetHead $(luciusFile  "templates/form.lucius")
                [whamlet|
                    <div class="background-list">
                        <div class="form">
                            <h1>
                                AGENDAMENTO
                            <br>
                            <br>
                            <h2>
                                Nome<br>
                            <h3>
                                #{petsNome petz}
                            <h2>
                                Idade<br>
                            <h3>
                                #{petsIdade petz}
                            <h2>
                                Motivo da Visita<br>
                            <h3>
                                #{petsMotivoVisita petz}
                            <br>
                            <div class="success">
                                Seu agendamento foi realizado com sucesso!<br>
                            <h5>Não se esqueça de conferir os horários de atendimento dos nossos especialistas na página veterinários.
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
                toWidgetHead $(luciusFile  "templates/form.lucius")
                [whamlet|
                    <div class="background-list">
                        <h1>
                            PETS CADASTRADOS
                        <br>
                        <br>
                        <form action=@{PetR} method=get>
                            <input class="btnAdd" type="submit" value="Adicionar Pet">
                        <table>
                            <thead>
                                <tr>
                                    <th class="col1"> 
                                        Nome do Pet
                                    
                                    <th class="col2">
                                        Idade
                                    
                                    <th class="col3">
                                        Motivo da Consulta

                                    <th class="col4">
                                        
                                    <th class="col5">
                                    
                                    <th class="col6">
                                        
                            <tbody>
                                $forall Entity pid p <- pets
                                    <tr>
                                        <td>
                                            <a href=@{DescPetR pid}>
                                            #{petsNome p}
                                        
                                        <td>
                                            #{petsIdade p}

                                        <td class="desc">
                                            #{petsMotivoVisita p}

                                        <th>
                                            <form action=@{UpdPetR pid} method=get>
                                                <input type="submit" value="Editar">

                                        <th>
                                            <form action=@{DescPetR pid} method=get>
                                                <input type="submit" value="Atender">

                                        <th>
                                            <form action=@{DelPetR pid} method=post>
                                                <input type="submit" value="Deletar">
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
                    redirect ListPetR 
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