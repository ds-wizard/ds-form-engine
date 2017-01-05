{-# LANGUAGE OverloadedStrings, NamedFieldPuns, RecordWildCards #-}

module FormEngine.FormElement.Validation where

import Prelude
import           Data.Maybe (isJust)

import FormEngine.FormElement.FormElement as Element
import FormEngine.FormItem

validateElements :: [FormElement] -> Bool
validateElements elements = all (== True) $ map validateElement $ filter toValidate elements
  where 
  toValidate :: FormElement -> Bool
  toValidate OptionalGroupElem{} = True -- OptionalGroup needs to be checked specially
  toValidate element = Element.isMandatory element

validateElement :: FormElement -> Bool
validateElement ChapterElem{ chElements, .. } = validateElements chElements
validateElement StringElem{ seValue, .. } = seValue /= ""
validateElement TextElem{ teValue, .. } = teValue /= ""
validateElement EmailElem{ eeValue, .. } = eeValue /= ""
validateElement element@NumberElem{ neMaybeValue, neMaybeUnitValue, .. } = valueOK && unitOK
  where
  valueOK :: Bool
  valueOK = case neMaybeValue of
    Just neValue -> neValue >= 0   
    Nothing -> False
  unitOK :: Bool
  unitOK = case nfiUnit $ formItem element of
    NoUnit -> True
    SingleUnit _ -> True
    MultipleUnit _ -> isJust neMaybeUnitValue
validateElement ChoiceElem{ chefi, cheOptions, .. } =
  let
    selectionOK = if isItemMandatory chefi then any optionElemIsSelected cheOptions else True
    itemsOK = all (== True) $ map validateChoiceElement cheOptions
      where
      validateChoiceElement :: OptionElement -> Bool
      validateChoiceElement SimpleOptionElem{} = True
      validateChoiceElement DetailedOptionElem{ dcheElements, .. } = validateElements dcheElements
  in
    selectionOK && itemsOK
validateElement ListElem{ leMaybeValue, .. } = isJust leMaybeValue 
validateElement SimpleGroupElem{ sgeElements, .. } = validateElements sgeElements
validateElement OptionalGroupElem{ ogeChecked, ogeElements, .. } 
  | ogeChecked = validateElements ogeElements
  | otherwise = True
validateElement MultipleGroupElem{ mgeElements, .. } = validateElements mgeElements
validateElement _ = True
