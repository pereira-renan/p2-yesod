{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Home where

import Foundation
import Yesod.Core
import Data.Text

getHomeR :: Handler Html
getHomeR = defaultLayout $ do 
    toWidgetHead [lucius|
        h1{
             color: red;
        }
    |]
    [whamlet|
         <h1>
             OLA MUNDO
            
         <a href=@{OutraR}> 
             OUTRA ROTA 
             
         <a href=@{SomarR 0 1}>
             SOMAR
        
         <a href=@{OlaR "Mundo!"}>
             OLA
    |]

getOutraR :: Handler Html
getOutraR = defaultLayout $ do 
    [whamlet|
         <div>
             OUTRA ROTA
             
         <a href=@{HomeR}> 
             VOLTAR 
    |]

getOlaR :: Text -> Handler Html
getOlaR nome = defaultLayout $ do 
    [whamlet|
         <h1>
            OLA #{nome}
    |]


    
