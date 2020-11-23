{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Consulta where

import Import
import Tool
import Text.Lucius
import Database.Persist.Sql

getListConsultaR :: Handler Html
getListConsultaR = do
    sess <- lookupSession "_EMAIL"
    valid <- lookupSession "_ID"
    case sess of 
        Nothing -> redirect HomeR
        Just email -> do
            usu <- runDB $ getBy (UniqueEmail email)
            case usu of 
                 Nothing -> redirect HomeR 
                 Just (Entity uid usuario) -> do 
                     let sql = "SELECT ??,??,?? FROM usuario \
                        \ INNER JOIN consulta ON consulta.usuarioid = usuario.id \
                        \ INNER JOIN pets ON consulta.petid = pets.id \
                        \ ORDER BY usuario.id = ?;"
                     pets <- runDB $ rawSql sql [toPersistValue uid] :: Handler [(Entity Usuario,Entity Consulta,Entity Pets)]
                     defaultLayout $ do 
                        toWidgetHead $(luciusFile  "templates/header.lucius")
                        $(whamletFile "templates/header.hamlet")
                        toWidgetHead $(luciusFile  "templates/form.lucius")
                        [whamlet|
                            <div class="background-list">
                                <h1>
                                    CONSULTAS
                                <h3>
                                    Olá #{usuarioNome usuario}!<br>Nesta página você pode acompanhar todas as consultas realizadas na nossa unidade, procure pelo nome do seu pet e verifique o seu diagnóstico!
                                <table>
                                    <thead>
                                        <tr>
                                            <th class="ccol1">
                                                Nome do Pet
                                            
                                            <th class="ccol2">
                                                Idade
                                            
                                            <th class="ccol3">
                                                Motivo da Consulta

                                            <th class="ccol5">
                                                Diagnóstico da Consulta
                                                
                                    <tbody>
                                        $forall (Entity _ usuario, Entity _ consulta, Entity _ pets) <- pets
                                            <tr>
                                                <td>
                                                    #{petsNome pets}
                                                
                                                <td>
                                                    #{petsIdade pets}

                                                <td class="desc">
                                                    #{petsMotivoVisita pets}

                                                <td class="desc">
                                                    #{consultaDesc consulta}
                        |]

postConsultarR :: PetsId -> Handler Html
postConsultarR pid = do
    sess <- lookupSession "_EMAIL"
    case sess of 
        Nothing -> redirect ForbiddenR
        Just _ -> do
            ((resp,_),_) <- runFormPost formDesc
            case resp of 
                FormSuccess desc -> do 
                    sess <- lookupSession "_EMAIL"
                    case sess of 
                        Nothing -> redirect HomeR
                        Just email -> do 
                            usuario <- runDB $ getBy (UniqueEmail email)
                            case usuario of 
                                Nothing -> redirect HomeR 
                                Just (Entity uid _) -> do 
                                    _ <- runDB $ insert (Consulta uid pid desc)
                                    redirect ListConsultaR
                _ -> redirect HomeR