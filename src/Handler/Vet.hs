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
formVet v = renderDivs $ Vets  
    <$> areq textField (FieldSettings "Nome: " 
                                      Nothing
                                      (Just "hs12")
                                      Nothing
                                      [("class","myClass")]
                       ) (fmap vetsNome v)
    <*> areq textField (FieldSettings "Especialidade: " 
                                      Nothing
                                      (Just "hs12")
                                      Nothing
                                      [("class","myClass")]
                       ) (fmap vetsEspecialidade v)

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
                [whamlet|
                    $if null valid
                        <li>
                            Você não tem permissão para acessar essa página.
                    $else
                        <h1>
                            CADASTRO DE VETERINARIOS
                        
                        <form action=@{rt} method=post>
                            ^{widget}
                            <input type="submit" value="Cadastrar">
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
                    redirect (DescVetR vid)
                _ -> redirect HomeR

-- SELECT * from vetz where id = vid 
getDescVetR :: VetsId -> Handler Html
getDescVetR vid = do 
    sess <- lookupSession "_ID"
    case sess of 
        Nothing -> redirect ForbiddenR
        Just _ -> do
            vetz <- runDB $ get404 vid
            valid <- lookupSession "_ID"
            defaultLayout $ do 
                sess <- lookupSession "_EMAIL"
                valid <- lookupSession "_ID"
                toWidgetHead $(luciusFile  "templates/header.lucius")
                $(whamletFile "templates/header.hamlet")
                [whamlet|
                    <h1>
                        Nome: #{vetsNome vetz}
                    
                    <h2>
                        Especialiade: #{vetsEspecialidade vetz}
                |]

getListVetR :: Handler Html
getListVetR = do 
        vets <- runDB $ selectList [] [Desc VetsEspecialidade]
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
                                Vets
                            
                            <th>
                            
                            <th>
                    <tbody>
                        $forall Entity vid v <- vets
                            <tr>
                                <td>
                                    #{vetsNome v}
                                
                                <td>
                                    #{vetsEspecialidade v}
                                
                                <th>
                                    <a href=@{UpdVetR vid}>
                                        Editar
                                $if null valid
                                $else
                                <th>
                                    <form action=@{DelVetR vid} method=post>
                                        <input type="submit" value="X">
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
                    redirect (DescVetR vid) 
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