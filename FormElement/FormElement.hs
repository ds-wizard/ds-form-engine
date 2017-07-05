{-# LANGUAGE OverloadedStrings, NamedFieldPuns, RecordWildCards, FlexibleInstances, TypeSynonymInstances, CPP #-}

module FormEngine.FormElement.FormElement where

import           Prelude
import           Data.Maybe (fromMaybe, mapMaybe, isNothing)
import           Data.Monoid ((<>))

import           FormEngine.FormItem
import           FormEngine.FormData

--import Debug.Trace

#ifdef __HASTE__
type Text = String
pack :: a -> a
pack = id
#else
import           Data.Text.Lazy (Text, pack)
--import qualified Data.Text.Lazy as T
#endif

---------------------------------
--import Debug.Hood.Observe
--import GHC.Generics
---------------------------------

data OptionElement = SimpleOptionElem { schi :: Option, scheSelected :: Bool }
                   | DetailedOptionElem { dchi :: Option, dcheSelected :: Bool , dcheElements :: [FormElement] }
--instance Observable OptionElement

instance Show OptionElement where
  show oe@SimpleOptionElem{} = "SimpleOptionElem { optionElemValue=" <> show (optionElemValue oe) <> ", scheSelected=" <> show (scheSelected oe) <> " }"
  show oe@DetailedOptionElem{} = "DetailedOptionElem { optionElemValue=" <> show (optionElemValue oe) <> ", dcheSelected=" <> show (dcheSelected oe)  <> ", dcheElements=" <> show (dcheElements oe) <>" }"

instance Eq OptionElement where
  oe1 == oe2 = optionItem oe1 Prelude.== optionItem oe2

optionItem :: OptionElement -> Option
optionItem SimpleOptionElem{ schi, .. } = schi
optionItem DetailedOptionElem{ dchi, .. } = dchi

optionElemValue :: OptionElement -> Text
optionElemValue = optionValue . optionItem

optionElemIsSelected :: OptionElement -> Bool
optionElemIsSelected SimpleOptionElem{ scheSelected, .. } = scheSelected
optionElemIsSelected DetailedOptionElem{ dcheSelected, .. } = dcheSelected

type ElemGroupNo = Int
data ElemGroup = ElemGroup { egElements :: [FormElement], egNumber :: ElemGroupNo } deriving (Show)
--instance Observable ElemGroup

data FormElement = ChapterElem { chfi :: FormItem, chElements :: [FormElement], visited :: Bool }
                 | StringElem { sfi :: FormItem, seValue :: Text, seGroupNo :: Maybe ElemGroupNo, seParent :: FormElement }
                 | TextElem { tfi :: FormItem, teValue :: Text, teGroupNo :: Maybe ElemGroupNo, teParent :: FormElement }
                 | EmailElem { efi :: FormItem, eeValue :: Text, eeGroupNo :: Maybe ElemGroupNo, eeParent :: FormElement }
                 | NumberElem { nfi :: FormItem, neMaybeValue :: Maybe Float, neMaybeUnitValue :: Maybe Text, neGroupNo :: Maybe ElemGroupNo, neParent :: FormElement }
                 | InfoElem { ifi :: FormItem, ieParent :: FormElement }
                 | ChoiceElem { chefi :: FormItem, cheOptions :: [OptionElement], cheGroupNo :: Maybe ElemGroupNo, cheParent :: FormElement }
                 | ListElem { lfi :: FormItem, leMaybeValue :: Maybe Text, leGroupNo :: Maybe ElemGroupNo, leParent :: FormElement }
                 | SimpleGroupElem { sgi :: FormItem, sgeElements :: [FormElement], sgeGroupNo :: Maybe ElemGroupNo, sgeParent :: FormElement }
                 | OptionalGroupElem { ogi :: FormItem, ogeChecked :: Bool, ogeElements :: [FormElement], ogeGroupNo :: Maybe ElemGroupNo, ogeParent :: FormElement }
                 | MultipleGroupElem { mgi :: FormItem, mgeGroups :: [ElemGroup], mgeGroupNo :: Maybe ElemGroupNo, mgeParent :: FormElement }
                 | SaveButtonElem { svi :: FormItem, svParent :: FormElement }
                 | SubmitButtonElem { sbi :: FormItem, sbParent :: FormElement }
--instance Observable FormElement

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

mGroupNo :: FormElement -> Maybe ElemGroupNo
mGroupNo ChapterElem{..} = Nothing
mGroupNo StringElem{ seGroupNo, .. } = seGroupNo
mGroupNo TextElem{ teGroupNo, .. } = teGroupNo
mGroupNo EmailElem{ eeGroupNo, .. } = eeGroupNo
mGroupNo NumberElem{ neGroupNo, .. } = neGroupNo
mGroupNo ChoiceElem{ cheGroupNo, .. } = cheGroupNo
mGroupNo InfoElem{..} = Nothing
mGroupNo ListElem{ leGroupNo, .. } = leGroupNo
mGroupNo SimpleGroupElem{ sgeGroupNo, .. } = sgeGroupNo
mGroupNo OptionalGroupElem{ ogeGroupNo, .. } = ogeGroupNo
mGroupNo MultipleGroupElem{ mgeGroupNo, .. } = mgeGroupNo
mGroupNo SaveButtonElem{..} = Nothing
mGroupNo SubmitButtonElem{..} = Nothing

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
  show e@ChapterElem{..} = "ChapterElem { id=" <> show (elementId e) <> ", children=" <> show (FormEngine.FormElement.FormElement.children e) <> " }"
  show e@StringElem{ seValue, .. } = "StringElem { id=" <> show ( elementId e) <> ", mGroupNo=" <> show (mGroupNo e) <> ", value=" <> show seValue <> " }"
  show e@TextElem{ teValue, .. } = "TextElem { id=" <> show ( elementId e) <> ", mGroupNo=" <> show (mGroupNo e) <> ", value=" <> show teValue <> " }"
  show e@EmailElem{ eeValue, .. } = "EmailElem { id=" <> show ( elementId e) <> ", mGroupNo=" <> show (mGroupNo e) <> ", value=" <> show eeValue <> " }"
  show e@NumberElem{ neMaybeValue, .. } = "NumberElem { id=" <> show ( elementId e) <> ", mGroupNo=" <> show (mGroupNo e) <> ", value=" <> show neMaybeValue <> ", unit=" <> show neMaybeUnitValue <> " }"
  show e@ChoiceElem{..} = "ChoiceElem { id=" <> show ( elementId e) <> ", cheOptions=" <> show cheOptions <> ", mGroupNo=" <> show (mGroupNo e) <> " }"
  show e@InfoElem{..} = "InfoElem { id=" <> show ( elementId e) <> " }"
  show e@ListElem{ leMaybeValue, .. } = "ListElem { id=" <> show ( elementId e) <> ", mGroupNo=" <> show (mGroupNo e) <> ", value=" <> show leMaybeValue <> " }"
  show e@SimpleGroupElem{..} = "SimpleGroupElem { id=" <> show ( elementId e) <> ", mGroupNo=" <> show (mGroupNo e) <> ", children=" <> show (FormEngine.FormElement.FormElement.children e) <> " }"
  show e@OptionalGroupElem{..} = "OptionalGroupElem { id=" <> show ( elementId e) <> ", mGroupNo=" <> show (mGroupNo e) <> ", children=" <> show (FormEngine.FormElement.FormElement.children e) <> " }"
  show e@MultipleGroupElem{..} = "MultipleGroupElem { id=" <> show ( elementId e) <> ", mGroupNo=" <> show (mGroupNo e) <> ", mgeGroups=" <> show mgeGroups <> " }"
  show e@SaveButtonElem{..} = "SaveButtonElem { id=" <> show ( elementId e) <> " }"
  show e@SubmitButtonElem{..} = "SubmitButtonElem { id=" <> show ( elementId e) <> " }"

elementRawId :: FormElement -> Text
elementRawId element = fiId $ formItem element

elementGroupId :: FormElement -> Text
elementGroupId element = case mGroupNo element of
  Nothing -> ""
  Just gNo -> "G" <> (pack . show) gNo

elementId :: FormElement -> Text
elementId element
  | isNothing $ mGroupNo element = elementRawId element
  | otherwise = elementRawId element <> "_" <> elementGroupId element

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
  children MultipleGroupElem{ mgeGroups, .. } = concatMap egElements mgeGroups
  children ChoiceElem{ cheOptions, .. } = foldl (\res opt -> res ++ FormEngine.FormElement.FormElement.children opt) [] cheOptions
  children _ = []

elemChapter :: FormElement -> FormElement
elemChapter element@ChapterElem{} = element
elemChapter element = elemChapter $ parentElem element

isMandatory :: FormElement -> Bool
isMandatory = isItemMandatory . formItem

maybeLabel :: FormElement -> Maybe Text
maybeLabel = iLabel . fiDescriptor . formItem

tags :: FormElement -> [Tag]
tags = iTags . fiDescriptor . formItem

level :: FormElement -> Int
level = fiLevel . formItem

numbering :: FormElement -> Numbering
numbering = fiNumbering . formItem

identity :: FormElement -> Text
identity = fromMaybe "" . iIdent . fiDescriptor . formItem

rules :: FormElement -> [FormRule]
rules = iRules . fiDescriptor . formItem

maybeLink :: FormElement -> Maybe Text
maybeLink = iLink . fiDescriptor . formItem

maybeStr2maybeFloat :: Maybe Text -> Maybe Float
maybeStr2maybeFloat ms = ms >>= str2maybeFloat
  where
    str2maybeFloat :: Text -> Maybe Float
    str2maybeFloat s =
      let conv = reads (show s) :: [(Float, String)]
      in case conv of
        []         -> Nothing
        [(res, _)] -> Just res
        (_, _) : (_ : _) -> Nothing

strValue :: FormElement -> Text
strValue StringElem { seValue, .. } = seValue
strValue TextElem { teValue, .. } = teValue
strValue EmailElem { eeValue, .. } = eeValue
strValue NumberElem { neMaybeValue, .. } = pack $ fromMaybe "" (show <$> neMaybeValue)
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
  , seGroupNo = egNumber <$> maybeGroup
  , seParent = parent1
  }
makeElem parent1 maybeGroup maybeFormData item@TextFI{} = Just TextElem
  { tfi = item
  , teValue = fromMaybe "" (getMaybeFFItemValue item maybeFormData)
  , teGroupNo = egNumber <$> maybeGroup
  , teParent = parent1
  }
makeElem parent1 maybeGroup maybeFormData item@EmailFI{} = Just EmailElem
  { efi = item
  , eeValue = fromMaybe "" (getMaybeFFItemValue item maybeFormData)
  , eeGroupNo = egNumber <$> maybeGroup
  , eeParent = parent1
  }
makeElem parent1 maybeGroup maybeFormData item@NumberFI{} = Just NumberElem
  { nfi = item
  , neMaybeValue = maybeStr2maybeFloat $ getMaybeFFItemValue item maybeFormData
  , neMaybeUnitValue = getMaybeNumberFIUnitValue item maybeFormData
  , neGroupNo = egNumber <$> maybeGroup
  , neParent = parent1
  }
makeElem parent1 maybeGroup maybeFormData item@ChoiceFI{ chfiAvailableOptions, .. } = Just choiceElem
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
          , dcheElements = mapMaybe (makeElem choiceElem maybeGroup maybeFormData) detailElements
          }
makeElem parent1 _ _ item@InfoFI{} = Just InfoElem
  { ifi = item
  , ieParent = parent1
  }
makeElem parent1 maybeGroup maybeFormData item@ListFI{} = Just ListElem
  { lfi = item
  , leMaybeValue = getMaybeFFItemValue item maybeFormData
  , leGroupNo = egNumber <$> maybeGroup
  , leParent = parent1
  }
makeElem parent1 maybeGroup maybeFormData item@SimpleGroup{ sgItems, .. } = Just simpleGroupElem
  where
  simpleGroupElem = SimpleGroupElem
    { sgi = item
    , sgeElements = items
    , sgeGroupNo = egNumber <$> maybeGroup
    , sgeParent = parent1
    }
  items = mapMaybe (makeElem simpleGroupElem maybeGroup maybeFormData) sgItems
makeElem parent1 maybeGroup maybeFormData item@OptionalGroup{ ogItems, .. } = Just optionalGroupElem
  where
  optionalGroupElem = OptionalGroupElem
    { ogi = item
    , ogeChecked = isCheckboxChecked item maybeFormData
    , ogeElements = items
    , ogeGroupNo = egNumber <$> maybeGroup
    , ogeParent = parent1
    }
  items = mapMaybe (makeElem optionalGroupElem maybeGroup maybeFormData) ogItems
makeElem parent1 maybeGroup maybeFormData item@MultipleGroup{ mgItems, .. } = Just multipleGroupElem
  where
  multipleGroupElem = MultipleGroupElem
    { mgi = item
    , mgeGroups = groups
    , mgeGroupNo = egNumber <$> maybeGroup
    , mgeParent = parent1
    }
  groups = let gData = getGroupData item maybeFormData in
    if null gData then
      [newGroup0]
    else
      map makeGroup (zip [0..] gData)
    where
      newGroup0 = ElemGroup{ egElements = items0, egNumber = 0 }
      items0 = mapMaybe (makeElem multipleGroupElem (Just newGroup0) Nothing) mgItems
      makeGroup :: (Int, FormData) -> ElemGroup
      makeGroup (gNo, groupFormData) = newGroup
        where
        newGroup = ElemGroup{ egElements = items, egNumber = gNo }
        items = mapMaybe (makeElem multipleGroupElem (Just newGroup) (Just groupFormData)) mgItems
makeElem parent1 _ _ item@SaveButtonFI{} = Just SaveButtonElem { svi = item, svParent = parent1 }
makeElem parent1 _ _ item@SubmitButtonFI{} = Just SubmitButtonElem { sbi = item, sbParent = parent1 }

numberElem2TB :: FormElement -> Maybe Float
numberElem2TB NumberElem{ neMaybeValue, neMaybeUnitValue, .. } =
  case neMaybeUnitValue of
    Just "MB" -> fmap (* 0.000001) neMaybeValue
    Just "GB" -> fmap (* 0.001) neMaybeValue
    Just "TB" -> fmap (* 1.0) neMaybeValue
    Just "PB" -> fmap (* 1000) neMaybeValue
    _ -> Nothing -- Nothing or unknown unit
numberElem2TB _ = Nothing
