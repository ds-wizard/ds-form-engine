{-# LANGUAGE OverloadedStrings, NamedFieldPuns, RecordWildCards, FlexibleInstances, TypeSynonymInstances #-}

module FormEngine.FormElement.FormElement where

import           Prelude
import           Data.Maybe (fromMaybe, mapMaybe, isNothing)
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

data ElemGroup = ElemGroup { egElements :: [FormElement], egNumber :: Int }

data FormElement = ChapterElem { chfi :: FormItem, chElements :: [FormElement], visited :: Bool }
                 | StringElem { sfi :: FormItem, seValue :: String, seGroup :: Maybe ElemGroup, seParent :: FormElement }
                 | TextElem { tfi :: FormItem, teValue :: String, teGroup :: Maybe ElemGroup, teParent :: FormElement }
                 | EmailElem { efi :: FormItem, eeValue :: String, eeGroup :: Maybe ElemGroup, eeParent :: FormElement }
                 | NumberElem { nfi :: FormItem, neMaybeValue :: Maybe Int, neMaybeUnitValue :: Maybe String , neGroup :: Maybe ElemGroup, neParent :: FormElement }
                 | InfoElem { ifi :: FormItem, ieParent :: FormElement }
                 | ChoiceElem { chefi :: FormItem, cheOptions :: [OptionElement], cheGroup :: Maybe ElemGroup, cheParent :: FormElement }
                 | ListElem { lfi :: FormItem, leMaybeValue :: Maybe String, leGroup :: Maybe ElemGroup, leParent :: FormElement }
                 | SimpleGroupElem { sgi :: FormItem, sgeElements :: [FormElement], sgeGroup :: Maybe ElemGroup, sgeParent :: FormElement }
                 | OptionalGroupElem { ogi :: FormItem, ogeChecked :: Bool, ogeElements :: [FormElement], ogeGroup :: Maybe ElemGroup, ogeParent :: FormElement }
                 | MultipleGroupElem { mgi :: FormItem, mgeGroups :: [ElemGroup], mgeGroup :: Maybe ElemGroup, mgeParent :: FormElement }
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

groupNo :: FormElement -> Maybe Int
groupNo ChapterElem{..} = Nothing
groupNo StringElem{ seGroup, .. } = egNumber <$> seGroup
groupNo TextElem{ teGroup, .. } = egNumber <$> teGroup
groupNo EmailElem{ eeGroup, .. } = egNumber <$> eeGroup
groupNo NumberElem{ neGroup, .. } = egNumber <$> neGroup
groupNo ChoiceElem{ cheGroup, .. } = egNumber <$> cheGroup
groupNo InfoElem{..}= Nothing
groupNo ListElem{ leGroup, .. } = egNumber <$> leGroup
groupNo SimpleGroupElem{ sgeGroup, .. } = egNumber <$> sgeGroup
groupNo OptionalGroupElem{ ogeGroup, .. } = egNumber <$> ogeGroup
groupNo MultipleGroupElem{ mgeGroup, .. } = egNumber <$> mgeGroup
groupNo SaveButtonElem{..} = Nothing
groupNo SubmitButtonElem{..} = Nothing

setGroup :: Maybe ElemGroup -> FormElement -> FormElement
setGroup _ element@ChapterElem{..} = element
setGroup group element@StringElem{..} = element { seGroup = group }
setGroup group element@TextElem{..} = element { teGroup = group }
setGroup group element@EmailElem{..} = element { eeGroup = group }
setGroup group element@NumberElem{..} = element { neGroup = group }
setGroup group element@ChoiceElem{..} = element { cheGroup = group }
setGroup _ element@InfoElem{..}= element
setGroup group element@ListElem{..} = element { leGroup = group }
setGroup group element@SimpleGroupElem{..} = element { sgeGroup = group }
setGroup group element@OptionalGroupElem{..} = element { ogeGroup = group }
setGroup group element@MultipleGroupElem{..} = element { mgeGroup = group }
setGroup _ element@SaveButtonElem{..} = element
setGroup _ element@SubmitButtonElem{..} = element

instance Show FormElement where
  show e@ChapterElem{..} = "ChapterElem id=" <> elementId e <> " children: " <> show (FormEngine.FormElement.FormElement.children e)
  show e@StringElem{ seValue, .. } = "StringElem id=" <> elementId e <> " value=" <> seValue
  show e@TextElem{ teValue, .. } = "TextElem id=" <> elementId e <> " value=" <> teValue
  show e@EmailElem{ eeValue, .. } = "EmailElem id=" <> elementId e <> " value=" <> eeValue
  show e@NumberElem{ neMaybeValue, .. } = "NumberElem id=" <> elementId e <> " value=" <> show neMaybeValue <> " unit=" <> show neMaybeUnitValue
  show e@ChoiceElem{..} = "ChoiceElem id=" <> elementId e
  show e@InfoElem{..} = "InfoElem id=" <> elementId e
  show e@ListElem{ leMaybeValue, .. } = "ListElem id=" <> elementId e <> " value=" <> show leMaybeValue
  show e@SimpleGroupElem{..} = "SimpleGroupElem id=" <> elementId e <> " children: " <> show (FormEngine.FormElement.FormElement.children e)
  show e@OptionalGroupElem{..} = "OptionalGroupElem id=" <> elementId e <> " children: " <> show (FormEngine.FormElement.FormElement.children e)
  show e@MultipleGroupElem{..} = "MultipleGroupElem id=" <> elementId e
  show e@SaveButtonElem{..} = "SaveButtonElem id=" <> elementId e
  show e@SubmitButtonElem{..} = "SubmitButtonElem id=" <> elementId e

elementId :: FormElement -> String
elementId element
  | isNothing $ groupNo element = fiId $ formItem element
  | otherwise = fiId (formItem element) <> "_G" <> fromMaybe "" (show <$> groupNo element)

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

instance HasChildren ElemGroup where
  children = egElements

instance HasChildren FormElement where
  children ChapterElem{ chElements, .. } = chElements
  children SimpleGroupElem{ sgeElements, .. } = sgeElements
  children OptionalGroupElem{ ogeElements, .. } = ogeElements
  children MultipleGroupElem{ mgeGroups, .. } = foldl (\res g -> res ++ egElements g) [] mgeGroups
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
  elems = mapMaybe (makeElem chapterElem Nothing maybeFormData) (chItems chapter)
makeChapter _ _ = Nothing

makeElem :: FormElement -> Maybe ElemGroup -> Maybe FormData -> FormItem -> Maybe FormElement
makeElem _ _ _ Chapter{} = Nothing -- Chapter can be just root element
makeElem parent1 maybeGroup maybeFormData item@StringFI{} = Just StringElem
  { sfi = item
  , seValue = fromMaybe "" (getMaybeFFItemValue item maybeFormData)
  , seGroup = maybeGroup
  , seParent = parent1
  }
makeElem parent1 maybeGroup maybeFormData item@TextFI{} = Just TextElem
  { tfi = item
  , teValue = fromMaybe "" (getMaybeFFItemValue item maybeFormData)
  , teGroup = maybeGroup
  , teParent = parent1
  }
makeElem parent1 maybeGroup maybeFormData item@EmailFI{} = Just EmailElem
  { efi = item
  , eeValue = fromMaybe "" (getMaybeFFItemValue item maybeFormData)
  , eeGroup = maybeGroup
  , eeParent = parent1
  }
makeElem parent1 maybeGroup maybeFormData item@NumberFI{} = Just NumberElem
  { nfi = item
  , neMaybeValue = maybeStr2maybeInt $ getMaybeFFItemValue item maybeFormData
  , neMaybeUnitValue = getMaybeNumberFIUnitValue item maybeFormData
  , neGroup = maybeGroup
  , neParent = parent1
  }
makeElem parent1 maybeGroup maybeFormData item@ChoiceFI{ chfiAvailableOptions, .. } = Just choiceElem
  where
    choiceElem = ChoiceElem
      { chefi = item
      , cheOptions = map makeElemChoiceElemI chfiAvailableOptions
      , cheGroup = maybeGroup
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
          , dcheElements = mapMaybe (makeElem choiceElem maybeGroup maybeFormData) detailElements
          }
makeElem parent1 _ _ item@InfoFI{} = Just InfoElem
  { ifi = item
  , ieParent = parent1
  }
makeElem parent1 maybeGroup maybeFormData item@ListFI{} = Just ListElem
  { lfi = item
  , leMaybeValue = getMaybeFFItemValue item maybeFormData
  , leGroup = maybeGroup
  , leParent = parent1
  }
makeElem parent1 maybeGroup maybeFormData item@SimpleGroup{ sgItems, .. } = Just simpleGroupElem
  where
  simpleGroupElem = SimpleGroupElem
    { sgi = item
    , sgeElements = items
    , sgeGroup = maybeGroup
    , sgeParent = parent1
    }
  items = mapMaybe (makeElem simpleGroupElem maybeGroup maybeFormData) sgItems
makeElem parent1 maybeGroup maybeFormData item@OptionalGroup{ ogItems, .. } = Just optionalGroupElem
  where
  optionalGroupElem = OptionalGroupElem
    { ogi = item
    , ogeChecked = isCheckboxChecked item maybeFormData
    , ogeElements = items
    , ogeGroup = maybeGroup
    , ogeParent = parent1
    }
  items = mapMaybe (makeElem optionalGroupElem maybeGroup maybeFormData) ogItems
makeElem parent1 maybeGroup maybeFormData item@MultipleGroup{ mgItems, .. } = Just multipleGroupElem
  where
  multipleGroupElem = MultipleGroupElem
    { mgi = item
    , mgeGroups = [group]
    , mgeGroup = maybeGroup
    , mgeParent = parent1
    }
  group = ElemGroup { egElements = items, egNumber = 0 }
  items = mapMaybe (makeElem multipleGroupElem (Just group) maybeFormData) mgItems
makeElem parent1 _ _ item@SaveButtonFI{} = Just SaveButtonElem { svi = item, svParent = parent1 }
makeElem parent1 _ _ item@SubmitButtonFI{} = Just SubmitButtonElem { sbi = item, sbParent = parent1 }

numberElem2TB :: FormElement -> Maybe Float
numberElem2TB NumberElem{ neMaybeValue, neMaybeUnitValue, .. } =
  case neMaybeUnitValue of
    Just "MB" -> fmap (* 0.000001) (fromIntegral <$> neMaybeValue :: Maybe Float)
    Just "GB" -> fmap (* 0.001) (fromIntegral <$> neMaybeValue :: Maybe Float)
    Just "TB" -> fmap (* 1.0) (fromIntegral <$> neMaybeValue :: Maybe Float)
    Just "PB" -> fmap (* 1000) (fromIntegral <$> neMaybeValue :: Maybe Float)
    _ -> Nothing -- Nothing or unknown unit
numberElem2TB _ = Nothing

