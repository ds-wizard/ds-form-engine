{-# LANGUAGE OverloadedStrings, NamedFieldPuns, RecordWildCards #-}

module FormElement.Identifiers where

import Prelude
import Data.Maybe (fromMaybe)
import Data.Monoid

import FormEngine.JQuery
import FormEngine.FormItem
import FormEngine.FormElement.FormElement as E

tabId :: FormElement -> Identifier
tabId element = "tab_" <> elementId element 

tabName :: FormElement -> String
tabName element = fromMaybe "" (iLabel $ fiDescriptor $ formItem element)

paneId :: FormElement -> Identifier
paneId element = "pane_" <> elementId element

diagramId :: FormElement -> Identifier
diagramId element = "diagram_" <> (elementId $ elemChapter element)

flagPlaceId :: FormElement -> Identifier
flagPlaceId element = elementId element <> "_flagPlaceId"

radioName :: FormElement -> String
radioName = elementId

radioId :: FormElement -> OptionElement -> Identifier
radioId element optionElem = radioName element <> "_" <> filter (\ch ->
  (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9') || ch == '_' || ch == '-')
  (optionElemValue optionElem)

optionSectionId :: FormElement -> OptionElement -> Identifier
optionSectionId element option = radioId element option <> "_detail"

checkboxId :: FormElement -> Identifier
checkboxId element = elementId element <> "_optional_group"

descSubpaneId :: FormElement -> Identifier
descSubpaneId element = (elementId $ elemChapter element) <> "_desc-subpane"

descSubpaneParagraphId :: FormElement -> Identifier
descSubpaneParagraphId element = (elementId $ elemChapter element) <> "_desc-subpane-text"


