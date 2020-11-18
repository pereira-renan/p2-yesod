{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Foundation where

import Yesod.Core
import Data.Text

data App = App

mkYesodData "App" $(parseRoutesFile "routes.yesodroutes")

instance Yesod App
