{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application where

import Foundation
import Yesod.Core

--- CADA ARQUIVO DE SRC DEVE SER INCLUIDO AQUI
import Home
import Add
---
mkYesodDispatch "App" resourcesApp
