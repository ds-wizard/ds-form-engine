{-# LANGUAGE OverloadedStrings #-}

module FormEngine.FormContext (FormContext(..), makeFormContext) where

import           FormEngine.FormElement.FormElement as E

data FormContext = FormContext { allElems :: [FormElement] }

makeFormContext :: [FormElement] -> FormContext
makeFormContext elems = FormContext { allElems = elems }
