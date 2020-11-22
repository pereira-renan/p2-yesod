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

formDesc :: Form Textarea
formDesc = renderBootstrap (areq textareaField "Diagnóstico da Consulta" Nothing)
