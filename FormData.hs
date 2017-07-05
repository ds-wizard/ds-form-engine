{-# LANGUAGE OverloadedStrings, CPP, NamedFieldPuns, RecordWildCards #-}

module FormEngine.FormData
  ( FieldInfo
  , FieldValue
  , FieldDatum
  , FormData
  , getFieldInfos
  , getName
  , getGroupNo
  , getMaybeFFItemValue
  , getMaybeNumberFIUnitValue
  , getGroupData
  , isCheckboxChecked
  , isOptionSelected
  , values2Data
  ) where

import Data.Monoid ((<>))
import qualified Data.List as DL
import FormEngine.FormItem
--import Debug.Trace

#ifndef __HASTE__
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
#else
import Prelude
type Text = String
#endif

---------------------------------
--import Debug.Hood.Observe
---------------------------------

type FieldInfo = (Text, Maybe Text) -- (name, mText)
type FieldValue = (Text, Maybe Text, Maybe Text) -- (name, mText, mValue)
type FieldDatum = (Text, Text)
type FormData = [FieldDatum]

getFieldInfos :: [FormItem] -> [FieldInfo]
getFieldInfos = foldl foldFieldInfo []
  where
  foldFieldInfo :: [FieldInfo] -> FormItem -> [FieldInfo]
  foldFieldInfo res item = res <> getFieldInfo
    where
    getFieldInfo = case item of
      StringFI{} -> [(fiId item, fiMaybeLabel item)]
      TextFI{} -> [(fiId item, fiMaybeLabel item)]
      EmailFI{}  -> [(fiId item, fiMaybeLabel item)]
      NumberFI{} -> (fiId item, fiMaybeLabel item) : unitField
        where
        unitField :: [FieldInfo]
        unitField = case nfiUnit item of
          NoUnit -> []
          SingleUnit _ -> []
          MultipleUnit _ -> [(nfiUnitId item, Nothing)]
      ChoiceFI{ chfiAvailableOptions, .. } -> (fiId item, fiMaybeLabel item) : choicesFields
        where
        choicesFields = foldl foldChoiceFields [] chfiAvailableOptions
          where
          foldChoiceFields :: [FieldInfo] -> Option -> [FieldInfo]
          foldChoiceFields res2 SimpleOption{} = res2
          foldChoiceFields res2 (DetailedOption _ _ items) = res2 ++ getFieldInfos items
      ListFI{} -> [(fiId item, fiMaybeLabel item)]
      Chapter{ chItems, .. } -> getFieldInfos chItems
      SimpleGroup{ sgItems, .. } -> getFieldInfos sgItems
      OptionalGroup{ ogItems, .. } -> (fiId item, fiMaybeLabel item) : getFieldInfos ogItems
      MultipleGroup{ mgItems, .. } -> getFieldInfos mgItems
      _ -> []

#ifdef __HASTE__
getName :: Text -> Text
getName key = case DL.elemIndex 'G' key of
  Nothing -> key
  Just i -> DL.take (i - 1) key
#else
getName :: Text -> Text
getName key = fst $ T.breakOn "_G" key
#endif

#ifdef __HASTE__
getGroupNo :: Text -> Int
getGroupNo key = case DL.elemIndex 'G' key of
  Nothing -> 0
  Just i -> if null readGno then 0 else fst $ head readGno
    where
    readGno = reads $ DL.drop (i + 1) key
#else
getGroupNo :: Text -> Int
getGroupNo key = if null readGno then 0 else fst $ head readGno
    where
    readGno = reads $ show $ snd $ T.breakOn "_G" key
#endif

getMaybeFFItemValue :: FormItem -> Maybe FormData -> Maybe Text
getMaybeFFItemValue item mFormData = --let mFormDataTr = if fiId item == "0_1_0_1_0_0_0_1_0_0_0_3_0_0_0_0_0" then traceShow mFormData mFormData else mFormData in
  case mFormData of
    Nothing -> Nothing
    Just formData -> case DL.find (\(k, _) -> getName k == fiId item) formData of
      Nothing -> Nothing
      Just (_, v) -> Just v

getMaybeNumberFIUnitValue :: FormItem -> Maybe FormData -> Maybe Text
getMaybeNumberFIUnitValue item mFormData = case mFormData of
  Nothing -> Nothing
  Just formData -> let unit = nfiUnit item in
    case unit of
      NoUnit -> Nothing
      SingleUnit l -> Just l
      MultipleUnit _ -> case DL.find (\(k, _) -> getName k == nfiUnitId item) formData of
        Nothing -> Nothing
        Just (_, v) -> Just v

getGroupData :: FormItem -> Maybe FormData -> [FormData]
getGroupData multipleGroup@MultipleGroup{} mFormData = case mFormData of
  Nothing -> []
  Just formData -> map mkGroup groups
    where
      fd1 :: FormData
      fd1 = foldMap (\item ->
        filter (\(k, _) -> getName k == fiId item) formData
        ) (childrenClosure multipleGroup)
      groups :: [Int]
      groups = if null fd1 then [] else [0..(DL.maximum $ map (getGroupNo . fst) fd1)]
      mkGroup :: Int -> FormData
      mkGroup groupNo = filter (\(k, _) -> getGroupNo k == groupNo) fd1
getGroupData _ _ = []

isCheckboxChecked :: FormItem -> Maybe FormData -> Bool
isCheckboxChecked item mFormData = let maybeRes = getMaybeFFItemValue item mFormData in
  case maybeRes of
    Nothing -> False
    Just val -> val == "on"

isOptionSelected :: Option -> FormItem -> Maybe FormData -> Bool
isOptionSelected option item mFormData = case getMaybeFFItemValue item mFormData of
  Nothing -> False
  Just selectedChoiceValue -> selectedChoiceValue == optionValue option

values2Data :: [FieldValue] -> FormData
values2Data = foldl foldValue []
  where
    foldValue :: FormData -> FieldValue -> FormData
    foldValue res (name, _, maybeValue) =
      case maybeValue of
        Nothing -> res
        Just value -> res ++ [(name, value)]
