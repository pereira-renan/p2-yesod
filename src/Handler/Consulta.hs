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
                                COMPRAS de #{usuarioNome usuario}
                            
                            <ul>
                                $forall (Entity _ _, Entity _ consulta, Entity _ pets) <- pets
                                    <li>
                                        O cliente {usuarioNome usuario} trouxe o pet #{petsNome pets} para consulta com X.
                                        Realizou uma consulta no valor de #{consultaVlpago consulta} e descreveu o seguinte diagnóstico:
        |]

postConsultarR :: PetsId -> Handler Html
postConsultarR pid = do
    ((resp,_),_) <- runFormPost formQt
    case resp of 
         FormSuccess qt -> do 
             sess <- lookupSession "_EMAIL"
             case sess of 
                  Nothing -> redirect HomeR
                  Just email -> do 
                      usuario <- runDB $ getBy (UniqueEmail email)
                      case usuario of 
                           Nothing -> redirect HomeR 
                           Just (Entity uid _) -> do 
                               _ <- runDB $ insert (Consulta uid pid qt)
                               redirect ListConsultaR
         _ -> redirect HomeR
