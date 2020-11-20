{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Consulta where

import Import
import Tool
import Database.Persist.Sql

getListConsultaR :: Handler Html
getListConsultaR = do
    sess <- lookupSession "_EMAIL"
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
                        \ WHERE usuario.id = ?"
                     pets <- runDB $ rawSql sql [toPersistValue uid] :: Handler [(Entity Usuario,Entity Consulta,Entity Pets)]
                     defaultLayout $ do 
                        [whamlet|
                            <h1>
                                Olá #{usuarioNome usuario}!
                            <br>    
                                Segue abaixo registro de consultas:
                            
                            <ul>
                                $forall (Entity _ _, Entity _ consulta, Entity _ pets) <- pets
                                    <li>
                                        O #{petsNome pets} deu entrada em consulta na unidade.
                                    <br>
                                        O veterinário escreveu o seguinte diagnóstico:
                                    <br>
                                        #{consultaDesc consulta}
        |]

postConsultarR :: PetsId -> Handler Html
postConsultarR pid = do
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
