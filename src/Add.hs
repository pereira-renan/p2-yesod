{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Add where

import Foundation
import Yesod.Core

getSomarR :: Int -> Int -> Handler Html
getSomarR n1 n2 = defaultLayout $ do 
    soma <- return (show (n1 + n2))
    [whamlet|
        <h1>
            A SOMA EH: #{soma}
    |]
