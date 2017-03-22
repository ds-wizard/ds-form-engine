{-# LANGUAGE OverloadedStrings, NamedFieldPuns, RecordWildCards #-}

module FormEngine.FormElement.Identifiers where

import Prelude
import Data.Text.Lazy (Text, pack)
import Data.Maybe (fromMaybe)
import Data.Monoid

import FormEngine.JQuery
import FormEngine.FormItem
import FormEngine.FormElement.FormElement as E

element2jq :: FormElement -> IO JQuery
element2jq element = selectByName $ show $ elementId element

tabId :: FormElement -> Identifier
tabId element = show $ "tab_" <> elementId element

tabName :: FormElement -> Text
tabName element = fromMaybe "" (iLabel $ fiDescriptor $ formItem element)

paneId :: FormElement -> Identifier
paneId element = show $ "pane_" <> elementId element

diagramId :: FormElement -> Identifier
diagramId element = show $ "diagram_" <> elementId (elemChapter element)

flagPlaceId :: FormElement -> Identifier
flagPlaceId element = show $ elementId element <> "_flagPlaceId"

radioName :: FormElement -> Text
radioName = elementId

radioId :: FormElement -> OptionElement -> Identifier
radioId element optionElem = show $ radioName element <> "_" <> pack (filter (\ch ->
  (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9') || ch == '_' || ch == '-')
  (show $ optionElemValue optionElem))

optionSectionId :: FormElement -> OptionElement -> Identifier
optionSectionId element option = radioId element option <> "_detail"

checkboxId :: FormElement -> Identifier
checkboxId element = show $ elementId element <> "_optional_group"

descSubpaneId :: FormElement -> Identifier
descSubpaneId element = show $ elementId (elemChapter element) <> "_desc-subpane"

descSubpaneParagraphId :: FormElement -> Identifier
descSubpaneParagraphId element = show $ elementId (elemChapter element) <> "_desc-subpane-text"


