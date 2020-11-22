{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Tool where

import Import
import Text.Lucius

geraForm :: Route App -> Text -> Text -> Maybe Html -> Widget -> Widget
geraForm rt titulo btn msg widget = $(whamletFile "templates/form.hamlet")

formDesc :: Form Text
formDesc = renderDivs (areq textField "Diagnóstico da Consulta: " Nothing)

formQt :: Form Int
formQt = renderDivs (areq intField "Idade: " Nothing) 
