{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Vet where

import Import
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
    (widget,_) <- generateFormPost (formVet vetz)
    defaultLayout $ do
        [whamlet|
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
    ((resp,_),_) <- runFormPost (formVet Nothing)
    case resp of 
         FormSuccess vetz -> do 
             vid <- runDB $ insert vetz
             redirect (DescVetR vid)
         _ -> redirect HomeR

-- SELECT * from vetz where id = vid 
getDescVetR :: VetsId -> Handler Html
getDescVetR vid = do 
    vetz <- runDB $ get404 vid
    valid <- lookupSession "_ID"
    defaultLayout [whamlet|
        $if null valid
            <li>
                Você não tem permissão para acessar essa página.
        $else
            <h1>
                Nome: #{vetsNome vetz}
            
            <h2>
                Especialiade: #{vetsEspecialidade vetz}
    |]

getListVetR :: Handler Html
getListVetR = do 
    -- vets :: [Entity Vets]
    vets <- runDB $ selectList [] [Desc VetsEspecialidade]
    defaultLayout [whamlet|
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
                            <th>
                                <form action=@{DelVetR vid} method=post>
                                    <input type="submit" value="X">
    |]

getUpdVetR :: VetsId -> Handler Html
getUpdVetR vid = do 
    antigo <- runDB $ get404 vid
    auxVetR (UpdVetR vid) (Just antigo)    
    
-- UPDATE vetz WHERE id = vid SET ...
postUpdVetR :: VetsId -> Handler Html
postUpdVetR vid = do
    ((resp,_),_) <- runFormPost (formVet Nothing)
    case resp of 
         FormSuccess novo -> do
            runDB $ replace vid novo
            redirect (DescVetR vid) 
         _ -> redirect HomeR

postDelVetR :: VetsId -> Handler Html
postDelVetR vid = do 
    _ <- runDB $ get404 vid 
    runDB $ delete vid 
    redirect ListVetR