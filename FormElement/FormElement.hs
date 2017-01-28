{-# LANGUAGE OverloadedStrings, NamedFieldPuns, RecordWildCards, FlexibleInstances, TypeSynonymInstances #-}

module FormEngine.FormElement.FormElement where

import           Prelude
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Monoid ((<>))

import           FormEngine.FormItem
import           FormEngine.FormData

data OptionElement = SimpleOptionElem { schi :: Option, scheSelected :: Bool }
                   | DetailedOptionElem { dchi :: Option, dcheSelected :: Bool , dcheElements :: [FormElement] }

instance Eq OptionElement where
  oe1 == oe2 = optionItem oe1 Prelude.== optionItem oe2

optionItem :: OptionElement -> Option
optionItem SimpleOptionElem{ schi, .. } = schi
optionItem DetailedOptionElem{ dchi, .. } = dchi

optionElemValue :: OptionElement -> String
optionElemValue = optionValue . optionItem

optionElemIsSelected :: OptionElement -> Bool
optionElemIsSelected SimpleOptionElem{ scheSelected, .. } = scheSelected
optionElemIsSelected DetailedOptionElem{ dcheSelected, .. } = dcheSelected

type ElemGroupNo = Int
data ElemGroup = ElemGroup { egElements :: [FormElement], egNumber :: ElemGroupNo }

data FormElement = ChapterElem { chfi :: FormItem, chElements :: [FormElement], visited :: Bool }
                 | StringElem { sfi :: FormItem, seValue :: String, seGroupNo :: Maybe ElemGroupNo, seParent :: FormElement }
                 | TextElem { tfi :: FormItem, teValue :: String, teGroupNo :: Maybe ElemGroupNo, teParent :: FormElement }
                 | EmailElem { efi :: FormItem, eeValue :: String, eeGroupNo :: Maybe ElemGroupNo, eeParent :: FormElement }
                 | NumberElem { nfi :: FormItem, neMaybeValue :: Maybe Int, neMaybeUnitValue :: Maybe String , neGroupNo :: Maybe ElemGroupNo, neParent :: FormElement }
                 | InfoElem { ifi :: FormItem, ieParent :: FormElement }
                 | ChoiceElem { chefi :: FormItem, cheOptions :: [OptionElement], cheGroupNo :: Maybe ElemGroupNo, cheParent :: FormElement }
                 | ListElem { lfi :: FormItem, leMaybeValue :: Maybe String, leGroupNo :: Maybe ElemGroupNo, leParent :: FormElement }
                 | SimpleGroupElem { sgi :: FormItem, sgeElements :: [FormElement], sgeGroupNo :: Maybe ElemGroupNo, sgeParent :: FormElement }
                 | OptionalGroupElem { ogi :: FormItem, ogeChecked :: Bool, ogeElements :: [FormElement], ogeGroupNo :: Maybe ElemGroupNo, ogeParent :: FormElement }
                 | MultipleGroupElem { mgi :: FormItem, mgeGroups :: [ElemGroup], mgeGroupNo :: Maybe ElemGroupNo, mgeParent :: FormElement }
                 | SaveButtonElem { svi :: FormItem, svParent :: FormElement }
                 | SubmitButtonElem { sbi :: FormItem, sbParent :: FormElement }

instance Eq FormElement where
  e1 == e2 = elementId e1 == elementId e2

formItem :: FormElement -> FormItem
formItem ChapterElem{ chfi, .. } = chfi
formItem StringElem{ sfi, .. } = sfi
formItem TextElem{ tfi, .. } = tfi
formItem EmailElem{ efi, .. } = efi
formItem NumberElem{ nfi, .. } = nfi
formItem ChoiceElem{ chefi, .. } = chefi
formItem InfoElem{ ifi, .. } = ifi
formItem ListElem{ lfi, .. } = lfi
formItem SimpleGroupElem{ sgi, .. } = sgi
formItem OptionalGroupElem{ ogi, .. } = ogi
formItem MultipleGroupElem{ mgi, .. } = mgi
formItem SaveButtonElem{ svi, .. } = svi
formItem SubmitButtonElem{ sbi, .. } = sbi

groupNo :: FormElement -> Maybe ElemGroupNo
groupNo ChapterElem{..} = Nothing
groupNo StringElem{ seGroupNo, .. } = seGroupNo
groupNo TextElem{ teGroupNo, .. } = teGroupNo
groupNo EmailElem{ eeGroupNo, .. } = eeGroupNo
groupNo NumberElem{ neGroupNo, .. } = neGroupNo
groupNo ChoiceElem{ cheGroupNo, .. } = cheGroupNo
groupNo InfoElem{..} = Nothing
groupNo ListElem{ leGroupNo, .. } = leGroupNo
groupNo SimpleGroupElem{ sgeGroupNo, .. } = sgeGroupNo
groupNo OptionalGroupElem{ ogeGroupNo, .. } = ogeGroupNo
groupNo MultipleGroupElem{ mgeGroupNo, .. } = mgeGroupNo
groupNo SaveButtonElem{..} = Nothing
groupNo SubmitButtonElem{..} = Nothing

setGroupOfElem :: Maybe ElemGroup -> FormElement -> FormElement
setGroupOfElem _ element@ChapterElem{..} = element
setGroupOfElem group element@StringElem{..} = element { seGroupNo = egNumber <$> group }
setGroupOfElem group element@TextElem{..} = element { teGroupNo = egNumber <$> group }
setGroupOfElem group element@EmailElem{..} = element { eeGroupNo = egNumber <$> group }
setGroupOfElem group element@NumberElem{..} = element { neGroupNo = egNumber <$> group }
setGroupOfElem group element@ChoiceElem{ cheOptions, .. } = element { cheGroupNo = egNumber <$> group, cheOptions = map (setGroupInOption group) cheOptions }
setGroupOfElem _ element@InfoElem{..} = element
setGroupOfElem group element@ListElem{..} = element { leGroupNo = egNumber <$> group }
setGroupOfElem group element@SimpleGroupElem{ sgeElements, .. } = element { sgeGroupNo = egNumber <$> group, sgeElements = map (setGroupOfElem group) sgeElements }
setGroupOfElem group element@OptionalGroupElem{ ogeElements, .. } = element { ogeGroupNo = egNumber <$> group, ogeElements = map (setGroupOfElem group) ogeElements }
setGroupOfElem group element@MultipleGroupElem{ mgeGroups, .. } = element { mgeGroupNo = egNumber <$> group, mgeGroups = map (setGroupInGroup group) mgeGroups }
setGroupOfElem _ element@SaveButtonElem{..} = element
setGroupOfElem _ element@SubmitButtonElem{..} = element

setGroupInOption :: Maybe ElemGroup -> OptionElement -> OptionElement
setGroupInOption _ e@SimpleOptionElem{..} = e
setGroupInOption group e@DetailedOptionElem{ dcheElements, .. } = e { dcheElements = map (setGroupOfElem group) dcheElements }

setGroupInGroup :: Maybe ElemGroup -> ElemGroup -> ElemGroup
setGroupInGroup parentGr group = group { egElements = map (setGroupOfElem parentGr) (egElements group) }

instance Show FormElement where
  show e@ChapterElem{..} = "ChapterElem id=" <> elementId e <> " children: " <> show (FormEngine.FormElement.FormElement.children e)
  show e@StringElem{ seValue, .. } = "StringElem id=" <> elementId e <> " groupNo=" <> show (groupNo e) <> " value=" <> seValue
  show e@TextElem{ teValue, .. } = "TextElem id=" <> elementId e <> " groupNo=" <> show (groupNo e) <> " value=" <> teValue
  show e@EmailElem{ eeValue, .. } = "EmailElem id=" <> elementId e <> " groupNo=" <> show (groupNo e) <> " value=" <> eeValue
  show e@NumberElem{ neMaybeValue, .. } = "NumberElem id=" <> elementId e <> " groupNo=" <> show (groupNo e) <> " value=" <> show neMaybeValue <> " unit=" <> show neMaybeUnitValue
  show e@ChoiceElem{..} = "ChoiceElem id=" <> elementId e <> " groupNo=" <> show (groupNo e)

elementId :: FormElement -> String
elementId = fiId . formItem

parentElem :: FormElement -> FormElement
parentElem element@ChapterElem{} = element
parentElem StringElem{ seParent, .. } = seParent
parentElem TextElem{ teParent, .. } = teParent
parentElem EmailElem{ eeParent, .. } = eeParent
parentElem NumberElem{ neParent, .. } = neParent
parentElem ChoiceElem{ cheParent, .. } = cheParent
parentElem InfoElem{ ieParent, .. } = ieParent
parentElem ListElem{ leParent, .. } = leParent
parentElem SimpleGroupElem{ sgeParent, .. } = sgeParent
parentElem OptionalGroupElem{ ogeParent, .. } = ogeParent
parentElem MultipleGroupElem{ mgeParent, .. } = mgeParent
parentElem SaveButtonElem{ svParent, .. } = svParent
parentElem SubmitButtonElem{ sbParent, .. } = sbParent

class HasChildren a where
  children :: a -> [FormElement]

instance HasChildren OptionElement where
  children SimpleOptionElem{} = []
  children DetailedOptionElem{ dcheElements, .. } = dcheElements

instance HasChildren FormElement where
  children ChapterElem{ chElements, .. } = chElements
  children SimpleGroupElem{ sgeElements, .. } = sgeElements
  children OptionalGroupElem{ ogeElements, .. } = ogeElements
  children MultipleGroupElem{ mgeElements, .. } = mgeElements
  children ChoiceElem{ cheOptions, .. } = foldl (\res opt -> res ++ FormEngine.FormElement.FormElement.children opt) [] cheOptions
  children _ = []

elemChapter :: FormElement -> FormElement
elemChapter element@ChapterElem{} = element
elemChapter element = elemChapter $ parentElem element

isMandatory :: FormElement -> Bool
isMandatory = isItemMandatory . formItem

maybeLabel :: FormElement -> Maybe String
maybeLabel = iLabel . fiDescriptor . formItem

tags :: FormElement -> [Tag]
tags = iTags . fiDescriptor . formItem

level :: FormElement -> Int
level = fiLevel . formItem

numbering :: FormElement -> Numbering
numbering = fiNumbering . formItem

identity :: FormElement -> String
identity = fromMaybe "" . iIdent . fiDescriptor . formItem

rules :: FormElement -> [FormRule]
rules = iRules . fiDescriptor . formItem

maybeLink :: FormElement -> Maybe String
maybeLink = iLink . fiDescriptor . formItem

maybeStr2maybeInt :: Maybe String -> Maybe Int
maybeStr2maybeInt ms = ms >>= str2maybeInt
  where
    str2maybeInt :: String -> Maybe Int
    str2maybeInt s =
      let conv = reads s :: [(Int, String)]
      in case conv of
        []         -> Nothing
        [(res, _)] -> Just res
        (_, _) : (_ : _) -> Nothing

strValue :: FormElement -> String
strValue StringElem { seValue, .. } = seValue
strValue TextElem { teValue, .. } = teValue
strValue EmailElem { eeValue, .. } = eeValue
strValue NumberElem { neMaybeValue, .. } = fromMaybe "" (show <$> neMaybeValue)
strValue ListElem { leMaybeValue, .. } = fromMaybe "" leMaybeValue
strValue _ = ""

makeChapter :: Maybe FormData -> FormItem -> Maybe FormElement
makeChapter maybeFormData chapter@Chapter{} = Just chapterElem
  where
  chapterElem = ChapterElem
    { chfi = chapter
    , chElements = elems
    , visited = False
    }
  elems = mapMaybe (makeElem chapterElem maybeFormData) (chItems chapter)
makeChapter _ _ = Nothing

makeElem :: FormElement -> Maybe FormData -> FormItem -> Maybe FormElement
makeElem _ _ Chapter{} = Nothing -- Chapter can be just root element
makeElem parent1 maybeFormData item@StringFI{} = Just StringElem
  { sfi = item
  , seValue = fromMaybe "" (getMaybeFFItemValue item maybeFormData)
  , seGroupNo = egNumber <$> maybeGroup
  , seParent = parent1
  }
makeElem parent1 maybeFormData item@TextFI{} = Just TextElem
  { tfi = item
  , teValue = fromMaybe "" (getMaybeFFItemValue item maybeFormData)
  , teGroupNo = egNumber <$> maybeGroup
  , teParent = parent1
  }
makeElem parent1 maybeFormData item@EmailFI{} = Just EmailElem
  { efi = item
  , eeValue = fromMaybe "" (getMaybeFFItemValue item maybeFormData)
  , eeGroupNo = egNumber <$> maybeGroup
  , eeParent = parent1
  }
makeElem parent1 maybeFormData item@NumberFI{} = Just NumberElem
  { nfi = item
  , neMaybeValue = maybeStr2maybeInt $ getMaybeFFItemValue item maybeFormData
  , neMaybeUnitValue = getMaybeNumberFIUnitValue item maybeFormData
  , neGroupNo = egNumber <$> maybeGroup
  , neParent = parent1
  }
makeElem parent1 maybeFormData item@ChoiceFI{ chfiAvailableOptions, .. } = Just choiceElem
  where
    choiceElem = ChoiceElem
      { chefi = item
      , cheOptions = map makeElemChoiceElemI chfiAvailableOptions
      , cheGroupNo = egNumber <$> maybeGroup
      , cheParent = parent1
      }
    makeElemChoiceElemI :: Option -> OptionElement
    makeElemChoiceElemI choice =
      case choice of
        selectionElem@(SimpleOption _) -> SimpleOptionElem
          { schi = selectionElem
          , scheSelected = isOptionSelected choice item maybeFormData
          }
        selectionElem@(DetailedOption _ _ detailElements) -> DetailedOptionElem
          { dchi = selectionElem
          , dcheSelected = isOptionSelected choice item maybeFormData
          , dcheElements = mapMaybe (makeElem choiceElem maybeFormData) detailElements
          }
makeElem parent1 _ item@InfoFI{} = Just InfoElem
  { ifi = item
  , ieParent = parent1
  }
makeElem parent1 maybeFormData item@ListFI{} = Just ListElem
  { lfi = item
  , leMaybeValue = getMaybeFFItemValue item maybeFormData
  , leGroupNo = egNumber <$> maybeGroup
  , leParent = parent1
  }
makeElem parent1 maybeFormData item@SimpleGroup{ sgItems, .. } = Just simpleGroupElem
  where
  simpleGroupElem = SimpleGroupElem
    { sgi = item
    , sgeElements = items
    , sgeGroupNo = egNumber <$> maybeGroup
    , sgeParent = parent1
    }
  items = mapMaybe (makeElem simpleGroupElem maybeFormData) sgItems
makeElem parent1 maybeFormData item@OptionalGroup{ ogItems, .. } = Just optionalGroupElem
  where
  optionalGroupElem = OptionalGroupElem
    { ogi = item
    , ogeChecked = isCheckboxChecked item maybeFormData
    , ogeElements = items
    , ogeGroupNo = egNumber <$> maybeGroup
    , ogeParent = parent1
    }
  items = mapMaybe (makeElem optionalGroupElem maybeFormData) ogItems
makeElem parent1 maybeFormData item@MultipleGroup{ mgItems, .. } = Just multipleGroupElem
  where
  multipleGroupElem = MultipleGroupElem
    { mgi = item
    , mgeGroups = [group]
    , mgeGroupNo = egNumber <$> maybeGroup
    , mgeElements = items
    , mgeParent = parent1
    }
  items = mapMaybe (makeElem multipleGroupElem maybeFormData) mgItems
makeElem parent1 _ item@SaveButtonFI{} = Just SaveButtonElem { svi = item, svParent = parent1 }
makeElem parent1 _ item@SubmitButtonFI{} = Just SubmitButtonElem { sbi = item, sbParent = parent1 }

numberElem2TB :: FormElement -> Maybe Float
numberElem2TB NumberElem{ neMaybeValue, neMaybeUnitValue, .. } =
  case neMaybeUnitValue of
    Just "MB" -> fmap (* 0.000001) (fromIntegral <$> neMaybeValue :: Maybe Float)
    Just "GB" -> fmap (* 0.001) (fromIntegral <$> neMaybeValue :: Maybe Float)
    Just "TB" -> fmap (* 1.0) (fromIntegral <$> neMaybeValue :: Maybe Float)
    Just "PB" -> fmap (* 1000) (fromIntegral <$> neMaybeValue :: Maybe Float)
    _ -> Nothing -- Nothing or unknown unit
numberElem2TB _ = Nothing

