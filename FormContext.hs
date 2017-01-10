{-# LANGUAGE OverloadedStrings #-}

module FormEngine.FormContext (FormContext(..)) where

import           FormEngine.FormElement.FormElement as E

data FormContext =
       FormContext
         { allElems :: [FormElement]
         , validImg :: String
         , invalidImg :: String
         , addImg :: String
         , removeImg :: String
         }