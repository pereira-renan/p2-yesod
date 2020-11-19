{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Pet where

import Import
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

auxPetR :: Route App -> Maybe Pets -> Handler Html
auxPetR rt petz = do
    (widget,_) <- generateFormPost (formPet petz)
    defaultLayout $ do
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
    ((resp,_),_) <- runFormPost (formPet Nothing)
    case resp of 
         FormSuccess petz -> do 
             pid <- runDB $ insert petz
             redirect (DescPetR pid)
         _ -> redirect HomeR

-- SELECT * from petz where id = pid 
getDescPetR :: PetsId -> Handler Html
getDescPetR pid = do 
    petz <- runDB $ get404 pid
    defaultLayout [whamlet|
        <h1>
            Nome: #{petsNome petz}
        
        <h2>
            Idade: #{petsIdade petz}
    |]

getListPetR :: Handler Html
getListPetR = do 
    -- pets :: [Entity Pets]
    pets <- runDB $ selectList [] [Desc PetsIdade]
    defaultLayout [whamlet|
            <table>
                <thead>
                    <tr>
                        <th> 
                            Nome
                        
                        <th>
                            Pets
                        
                        <th>
                        
                        <th>
                <tbody>
                    $forall Entity pid p <- pets
                        <tr>
                            <td>
                                #{petsNome p}
                            
                            <td>
                                #{petsIdade p}
                            
                            <th>
                                <a href=@{UpdPetR pid}>
                                    Editar
                            <th>
                                <form action=@{DelPetR pid} method=post>
                                    <input type="submit" value="X">
    |]

getUpdPetR :: PetsId -> Handler Html
getUpdPetR pid = do 
    antigo <- runDB $ get404 pid
    auxPetR (UpdPetR pid) (Just antigo)    
    
-- UPDATE petz WHERE id = pid SET ...
postUpdPetR :: PetsId -> Handler Html
postUpdPetR pid = do
    ((resp,_),_) <- runFormPost (formPet Nothing)
    case resp of 
         FormSuccess novo -> do
            runDB $ replace pid novo
            redirect (DescPetR pid) 
         _ -> redirect HomeR

postDelPetR :: PetsId -> Handler Html
postDelPetR pid = do 
    _ <- runDB $ get404 pid 
    runDB $ delete pid 
    redirect ListPetR



