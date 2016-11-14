{-# LANGUAGE OverloadedStrings #-}

module FormEngine.FormContext (FormContext(..), makeFormContext) where

import           FormElement.FormElement as E

data FormContext = FormContext { allElems :: [FormElement] }

makeFormContext :: [FormElement] -> FormContext
makeFormContext elems = FormContext { allElems = elems }
