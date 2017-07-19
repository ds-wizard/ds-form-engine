{-# LANGUAGE OverloadedStrings #-}

module FormEngine.FormContext where

import FormEngine.FormElement.FormElement (FormElement)

data FormContext =
       FormContext
         { allElems :: [FormElement]
         , validImg :: String
         , invalidImg :: String
         , addImg :: String
         , removeImg :: String
         }
