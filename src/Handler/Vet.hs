{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Vet where

import Import
import Text.Lucius
--import Database.Persist.Postgresql

-- (<$>) = fmap :: Functor f => (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
formVet :: Maybe Vets -> Form Vets
formVet v = renderBootstrap $ Vets  
    <$> areq textField (FieldSettings "Nome" 
                                      Nothing
                                      (Just "hs12")
                                      Nothing
                                      [("class","vetNome")]
                       ) (fmap vetsNome v)
    <*> areq textField (FieldSettings "Especialidade" 
                                      Nothing
                                      (Just "hs12")
                                      Nothing
                                      [("class","vetEspec")]
                       ) (fmap vetsEspecialidade v)
    <*> areq textField (FieldSettings "Rede Social" 
                                      Nothing
                                      (Just "hs12")
                                      Nothing
                                      [("class","vetRedes")]
                       ) (fmap vetsRedeSocial v)
    <*> areq textareaField (FieldSettings "Expediente" 
                                      Nothing
                                      (Just "hs12")
                                      Nothing
                                      [("class","vetExpediente")]
                       ) (fmap vetsExpediente v)

auxVetR :: Route App -> Maybe Vets -> Handler Html
auxVetR rt vetz = do
    sess <- lookupSession "_ID"
    case sess of 
        Nothing -> redirect ForbiddenR
        Just _ -> do
            (widget,_) <- generateFormPost (formVet vetz)
            valid <- lookupSession "_ID"
            defaultLayout $ do 
                sess <- lookupSession "_EMAIL"
                valid <- lookupSession "_ID"
                toWidgetHead $(luciusFile  "templates/header.lucius")
                $(whamletFile "templates/header.hamlet")
                toWidgetHead $(luciusFile  "templates/form.lucius")
                [whamlet|
                    <div class="background-list">
                        <div class="form">
                            $if null valid
                                <li>
                                    Você não tem permissão para acessar essa página.
                            $else
                                <h1>
                                    VETERINÁRIO
                                
                                <form action=@{rt} method=post>
                                    ^{widget}
                                    <input id="enviarVet" type="submit" value="Enviar">
                |]
    
getVetR :: Handler Html
getVetR = auxVetR VetR Nothing
    
postVetR :: Handler Html
postVetR = do
    sess <- lookupSession "_ID"
    case sess of 
        Nothing -> redirect ForbiddenR
        Just _ -> do
            ((resp,_),_) <- runFormPost (formVet Nothing)
            case resp of 
                FormSuccess vetz -> do 
                    vid <- runDB $ insert vetz
                    redirect ListVetR
                _ -> redirect HomeR

-- SELECT * from vetz where id = vid 
getDescVetR :: VetsId -> Handler Html
getDescVetR vid = do 
    sess <- lookupSession "_EMAIL"
    case sess of 
        Nothing -> redirect ForbiddenR
        Just _ -> do
            vetz <- runDB $ get404 vid
            valid <- lookupSession "_ID"
            defaultLayout $ do 
                setTitle "Veterinários"
                sess <- lookupSession "_EMAIL"
                valid <- lookupSession "_ID"
                toWidgetHead $(luciusFile  "templates/header.lucius")
                $(whamletFile "templates/header.hamlet")
                toWidgetHead $(luciusFile  "templates/form.lucius")
                [whamlet|
                    <div class="background-list">
                        <div class="form">
                            <h1>
                                NOSSOS VETERINÁRIOS
                            <br>
                            <h3 style="font-size: 24px">
                                Conheça mais sobre #{vetsNome vetz}!
                            <br>
                            <h2>
                                Especialidade<br>
                            <h3>
                                #{vetsEspecialidade vetz}<br><br>
                            <h2>
                                Rede Social<br>
                            <h3>
                                <a href="#{vetsRedeSocial vetz}">
                                    #{vetsRedeSocial vetz}<br><br>
                            <h2>
                                Expediente<br>
                            <h3>
                                #{vetsExpediente vetz}
                |]

getListVetR :: Handler Html
getListVetR = do 
        vets <- runDB $ selectList [] [Desc VetsEspecialidade]
        defaultLayout $ do 
            sess <- lookupSession "_EMAIL"
            valid <- lookupSession "_ID"
            setTitle "Veterinários Parceiros"
            toWidgetHead $(luciusFile  "templates/header.lucius")
            $(whamletFile "templates/header.hamlet")
            toWidgetHead $(luciusFile  "templates/form.lucius")
            [whamlet|
                <div class="background-list">
                    <div class="list">
                        <h1>
                            VETERINÁRIOS
                        <br>
                        <h3 class="intro">
                            Os veterinários abaixo são os nossos apaixonados por Pets, são eles que realizaram a consulta presencial em nossa unidade, conheça mais sobre eles, suas especialidades e seu expediente na unidade.
                        $maybe adm <- valid
                            <form action=@{VetR} method=get>
                                <input class="btnAdd" type="submit" value="Adicionar Veterinário">
                        $nothing
                        <table>
                                <thead>
                                    <tr>
                                        <th class="colv1">
                                            Nome do Vet
                                        
                                        <th class="colv2">
                                            Especialidades
                                        
                                        <th class="colv3">
                                            Rede Social

                                        <th class="colv4">
                                            Expediente

                                        <th class="colv5">
                                        
                                        <th class="colv6">
                                            
                                <tbody>
                                    $forall Entity vid v <- vets
                                        <tr>
                                            <td>
                                                <a href=@{DescVetR vid}>
                                                #{vetsNome v}
                                            
                                            <td>
                                                #{vetsEspecialidade v}

                                            <td>
                                                <a href=#{vetsRedeSocial v} target="_blank">
                                                    #{vetsRedeSocial v}

                                            <td>
                                                #{vetsExpediente v}

                                            $if null valid
                                                <th>
                                                    <form action=@{DescVetR vid} method=get>
                                                        <input type="submit" value="Ver">
                                                <th>
                                                    <form action=@{PetR} method=get>
                                                        <input type="submit" value="Agendar">
                                            $else
                                                <th>
                                                    <form action=@{UpdVetR vid} method=get>
                                                        <input type="submit" value="Editar">
                                                <th>
                                                    <form action=@{DelVetR vid} method=post>
                                                        <input type="submit" value="Deletar">                                    
            |]

getUpdVetR :: VetsId -> Handler Html
getUpdVetR vid = do 
    sess <- lookupSession "_ID"
    case sess of 
        Nothing -> redirect ForbiddenR
        Just _ -> do
            antigo <- runDB $ get404 vid
            auxVetR (UpdVetR vid) (Just antigo)    
    
-- UPDATE vetz WHERE id = vid SET ...
postUpdVetR :: VetsId -> Handler Html
postUpdVetR vid = do
    sess <- lookupSession "_ID"
    defaultLayout $ do 
        setTitle "Veterinários"
        sess <- lookupSession "_EMAIL"
        valid <- lookupSession "_ID"
        toWidgetHead $(luciusFile  "templates/header.lucius")
        $(whamletFile "templates/header.hamlet")
    case sess of 
        Nothing -> redirect ForbiddenR
        Just _ -> do
            ((resp,_),_) <- runFormPost (formVet Nothing)
            case resp of 
                FormSuccess novo -> do
                    runDB $ replace vid novo
                    redirect ListVetR 
                _ -> redirect HomeR

postDelVetR :: VetsId -> Handler Html
postDelVetR vid = do
    sess <- lookupSession "_ID"
    case sess of 
        Nothing -> redirect ForbiddenR
        Just _ -> do
            _ <- runDB $ get404 vid 
            runDB $ delete vid 
            redirect ListVetR