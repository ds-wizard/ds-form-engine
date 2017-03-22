{-# LANGUAGE OverloadedStrings, CPP, NamedFieldPuns, RecordWildCards #-}

module FormEngine.FormData (
  FieldInfo
, FieldValue
, FieldDatum
, FormData
, respondentKeyFieldId
, respondentKeyFieldName
, getFieldInfos
, getMaybeFFItemValue
, getMaybeFFKeyValue
, getMaybeNumberFIUnitValue
, getMaybeSelectedChoiceValue
, isCheckboxChecked
, isOptionSelected
, values2Data
) where

import FormEngine.FormItem

#ifdef __HASTE__
type Text = String
#else
import           Data.Text.Lazy (Text)
import           Prelude
#endif

type FieldInfo = (Text, Maybe Text) -- (name, mText)
type FieldValue = (Text, Maybe Text, Maybe Text) -- (name, mText, mValue)
type FieldDatum = (Text, Text)
type FormData = [FieldDatum]

respondentKeyFieldId :: Text
respondentKeyFieldId = "respondent_key_field"

respondentKeyFieldName :: Text
respondentKeyFieldName = "respondent_key"

getFieldInfos :: [FormItem] -> [FieldInfo]
getFieldInfos = foldl foldFieldInfo []
  where
  foldFieldInfo :: [FieldInfo] -> FormItem -> [FieldInfo]
  foldFieldInfo res item = res ++ getFieldInfo
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

getMaybeFFItemValue :: FormItem -> Maybe FormData -> Maybe Text
getMaybeFFItemValue item maybeFormData = case maybeFormData of
  Nothing -> Nothing
  Just formData -> lookup (fiId item) formData

getMaybeFFKeyValue :: Text -> Maybe FormData -> Maybe Text
getMaybeFFKeyValue key maybeFormData = case maybeFormData of
  Nothing -> Nothing
  Just formData -> lookup key formData

getMaybeNumberFIUnitValue :: FormItem -> Maybe FormData -> Maybe Text
getMaybeNumberFIUnitValue item maybeFormData = case maybeFormData of
  Nothing -> Nothing
  Just formData -> let unit = nfiUnit item in
    case unit of
      NoUnit -> Nothing
      SingleUnit l -> Just l
      MultipleUnit _ -> lookup (nfiUnitId item) formData

getMaybeSelectedChoiceValue :: FormItem -> Maybe FormData -> Maybe Text
getMaybeSelectedChoiceValue choiceFI = getMaybeFFKeyValue $ fiId choiceFI

isCheckboxChecked :: FormItem -> Maybe FormData -> Bool
isCheckboxChecked item maybeFormData = let maybeRes = getMaybeFFItemValue item maybeFormData in
  case maybeRes of
    Nothing -> False
    Just val -> val == "on"

isOptionSelected :: Option -> FormItem -> Maybe FormData -> Bool
isOptionSelected option item maybeFormData = case getMaybeSelectedChoiceValue item maybeFormData of
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
