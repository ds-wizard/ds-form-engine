{-# LANGUAGE OverloadedStrings, CPP, NamedFieldPuns, RecordWildCards #-}

module FormEngine.FormItem (
  Gender(..)
, Numbering(..)
, ItemIdentity
, Unit(..)
, Tag(..)
, tag2Text
, FIDescriptor(..)
, defaultFIDescriptor
, Option(..)
, optionValue
, FormItem(..)
, FormRule(..)
, isItemMandatory
, fiDescriptor
, fiNumbering
, fiId
, nfiUnitId
, numbering2text
, fiMaybeLabel
, fiLevel
, children
, childrenClosure
, prepareForm
) where

import           Data.Monoid ((<>))
--import Debug.Trace

#ifdef __HASTE__
import           Data.List (intercalate)
type Text = String
pack :: a -> a
pack = id
#else
import           Data.Text.Lazy (Text, pack, intercalate)
#endif

---------------------------------
--import Debug.Hood.Observe
--import GHC.Generics
--instance Observable Text where
--  observer = observeBase
---------------------------------

data Gender = Mr
            | Mrs

data Numbering = NoNumbering
               | Numbering [Int] Int -- current sections numbers and level
  deriving (Eq, Show)
--instance Observable Numbering

type ItemIdentity = Text

data Unit = SingleUnit Text
          | MultipleUnit [Text]
          | NoUnit
  deriving (Eq, Show)
--instance Observable Unit

newtype Tag = Tag Text
  deriving (Eq, Show)
--instance Observable Tag

tag2Text :: Tag -> Text
tag2Text (Tag text) = text

type Param = ItemIdentity

data FormRule = SumRule [Param] Param -- operands and result Identities
              | SumTBsRule [Param] Param
              | CopyValueRule Param Param
              | ReadOnlyRule
              | NumValueRule (Float -> Bool)
--instance Observable FormRule

instance Show FormRule where
  show (SumRule operands result) = "SumRule @ " ++ show operands ++ " -> " ++ show result
  show (SumTBsRule operands result) = "SumTBsRule @ " ++ show operands ++ " -> " ++ show result
  show (CopyValueRule operand result) = "CopyValueRule @ " ++ show operand ++ " -> " ++ show result
  show ReadOnlyRule = "ReadOnlyRule"
  show (NumValueRule _) = "NumValueRule (Float -> Bool)"

data FIDescriptor =
       FIDescriptor
         { iLabel :: Maybe Text
         , iNumbering :: Numbering
         , iIdent :: Maybe Text
         , iTags :: [Tag]
         , iShortDescription :: Maybe Text
         , iLongDescription :: Maybe Text
         , iLink :: Maybe Text
         , iMandatory :: Bool
         , iRules :: [FormRule]
         , iAutoComplete :: [Text]
         }
  deriving (Show)
--instance Observable FIDescriptor

defaultFIDescriptor :: FIDescriptor
defaultFIDescriptor = FIDescriptor
  { iLabel = Nothing
  , iNumbering = NoNumbering
  , iIdent = Nothing
  , iTags = []
  , iShortDescription = Nothing
  , iLongDescription = Nothing
  , iLink = Nothing
  , iMandatory = False
  , iRules = []
  , iAutoComplete = []
  }

data Option = SimpleOption Text
            | DetailedOption Numbering Text [FormItem]
  deriving (Show)
--instance Observable Option

instance Eq Option where
  o1 == o2 = optionValue o1 == optionValue o2 -- We assume comparing options just in one ChoiceFI

optionValue :: Option -> Text
optionValue (SimpleOption value) = value
optionValue (DetailedOption _ value _) = value

data FormItem = StringFI { sfiDescriptor :: FIDescriptor}
              | TextFI { tfiDescriptor :: FIDescriptor}
              | EmailFI { efiDescriptor :: FIDescriptor}
              | NumberFI { nfiDescriptor :: FIDescriptor, nfiUnit :: Unit}
              | InfoFI { ifiDescriptor :: FIDescriptor, ifiText :: Text }
              | ChoiceFI { chfiDescriptor :: FIDescriptor , chfiAvailableOptions :: [Option] }
              | ListFI { lfiDescriptor :: FIDescriptor , lfiAvailableOptions :: [(Text, Text)] }
              | Chapter { chDescriptor :: FIDescriptor, chItems :: [FormItem] }
              | SimpleGroup { sgDescriptor :: FIDescriptor, sgLevel :: Int, sgItems :: [FormItem] }
              | OptionalGroup { ogDescriptor :: FIDescriptor, ogLevel :: Int, ogItems :: [FormItem] }
              | MultipleGroup { mgDescriptor :: FIDescriptor, mgLevel :: Int, mgItems :: [FormItem] }
              | SaveButtonFI { sviDescriptor :: FIDescriptor }
              | SubmitButtonFI { sbiDescriptor :: FIDescriptor }
  deriving (Show)
--instance Observable FormItem

isItemMandatory :: FormItem -> Bool
isItemMandatory = iMandatory . fiDescriptor

fiDescriptor :: FormItem -> FIDescriptor
fiDescriptor StringFI{ sfiDescriptor, .. } = sfiDescriptor
fiDescriptor TextFI{ tfiDescriptor, .. } = tfiDescriptor
fiDescriptor EmailFI{ efiDescriptor, .. } = efiDescriptor
fiDescriptor NumberFI{ nfiDescriptor, .. } = nfiDescriptor
fiDescriptor ChoiceFI{ chfiDescriptor, .. } = chfiDescriptor
fiDescriptor ListFI{ lfiDescriptor, .. } = lfiDescriptor
fiDescriptor Chapter{ chDescriptor, .. } = chDescriptor
fiDescriptor SimpleGroup{ sgDescriptor, .. } = sgDescriptor
fiDescriptor OptionalGroup{ ogDescriptor, .. } = ogDescriptor
fiDescriptor MultipleGroup{ mgDescriptor, .. } = mgDescriptor
fiDescriptor SaveButtonFI{ sviDescriptor, .. } = sviDescriptor
fiDescriptor SubmitButtonFI{ sbiDescriptor, .. } = sbiDescriptor
fiDescriptor InfoFI { ifiDescriptor, .. } = ifiDescriptor

fiNumbering :: FormItem -> Numbering
fiNumbering = iNumbering . fiDescriptor

children :: FormItem -> [FormItem]
children Chapter{ chItems, .. } = chItems
children SimpleGroup{ sgItems, .. } = sgItems
children OptionalGroup{ ogItems, .. } = ogItems
children MultipleGroup{ mgItems, .. } = mgItems
children ChoiceFI{ chfiAvailableOptions, .. } = foldMap (\o ->
  case o of
    SimpleOption{} -> []
    DetailedOption _ _ items -> items
  ) chfiAvailableOptions
children _ = []

childrenClosure :: FormItem -> [FormItem]
childrenClosure item = [item] <> foldChildrenClosure
  where
  foldChildrenClosure = foldMap childrenClosure $ children item

fiLevel :: FormItem -> Int
fiLevel SimpleGroup{ sgLevel, .. } = sgLevel
fiLevel OptionalGroup{ ogLevel, .. } = ogLevel
fiLevel MultipleGroup{ mgLevel, .. } = mgLevel
fiLevel _ = 0

numbering2text :: Numbering -> Text
numbering2text NoNumbering = ""
numbering2text (Numbering array level) = intercalate "_" (map (pack . show) (take (level + 1) array))

fiId :: FormItem -> Text
fiId = numbering2text . fiNumbering

nfiUnitId :: FormItem -> Text
nfiUnitId nfiItem = fiId nfiItem <> "_unit"

fiMaybeLabel :: FormItem -> Maybe Text
fiMaybeLabel = iLabel . fiDescriptor

getLevel :: Numbering -> Int
getLevel NoNumbering = 0
getLevel (Numbering _ level) = level

--assignItemIdentifier :: FormItem -> Numbering -> FormItem
--assignItemIdentifier item numbering
--  | origId == ([], 0) = item { fiDescriptor = (fiDescriptor item) { iNumbering = makeIdentifier numbering } }
--  | otherwise = item
--  where
--    origId = fiId item

incrementAtLevel :: Numbering -> Numbering
incrementAtLevel NoNumbering = NoNumbering
incrementAtLevel (Numbering array level) = Numbering array2 level
  where
    array2 = take level array ++ [(array !! level) + 1] ++ repeat 0

incrementLevel :: Numbering -> Numbering
incrementLevel NoNumbering = NoNumbering
incrementLevel (Numbering array level) = Numbering array (level + 1)

numberItemInto :: (Numbering, [FormItem]) -> FormItem -> (Numbering, [FormItem])
numberItemInto (numbering, items) item = (numbering2, items2)
  where
    numbering2 = fst (incrementNumbering (numbering, item))
    items2 = items ++ [snd (incrementNumbering (numbering, item))]

numberChoiceInto :: (Numbering, [Option]) -> Option -> (Numbering, [Option])
numberChoiceInto (numbering, choices) (SimpleOption value) = (numbering, choices ++ [SimpleOption value])
numberChoiceInto (numbering, choices) (DetailedOption _ value items) = (numbering1, choices2)
  where
    numbering1 = incrementAtLevel numbering
    (_, items2) = foldl numberItemInto (incrementLevel numbering, []) items
    choices2 = choices ++ [DetailedOption numbering value items2]

incrementNumbering :: (Numbering, FormItem) -> (Numbering, FormItem)
incrementNumbering (numbering, item@StringFI{ .. }) = (incrementAtLevel numbering, item2)
  where
    item2 = item { sfiDescriptor = sfiDescriptor { iNumbering = numbering } }

incrementNumbering (numbering, item@TextFI{ .. }) = (incrementAtLevel numbering, item2)
  where
    item2 = item { tfiDescriptor = tfiDescriptor { iNumbering = numbering } }

incrementNumbering (numbering, item@EmailFI{ .. }) = (incrementAtLevel numbering, item2)
  where
    item2 = item { efiDescriptor = efiDescriptor { iNumbering = numbering } }

incrementNumbering (numbering, item@NumberFI{ .. }) = (incrementAtLevel numbering, item2)
  where
    item2 = item { nfiDescriptor = nfiDescriptor { iNumbering = numbering } }

incrementNumbering (numbering, item@InfoFI{ .. }) = (incrementAtLevel numbering, item2)
  where
    item2 = item { ifiDescriptor = ifiDescriptor { iNumbering = numbering } }

incrementNumbering (numbering, item@ChoiceFI{ chfiAvailableOptions, .. }) =
  (numbering1, item { chfiDescriptor = chfiDescriptor { iNumbering = numbering }
                    , chfiAvailableOptions = choices2
                    })
  where
    numbering1 = incrementAtLevel numbering
    choices2 = snd $ foldl numberChoiceInto (incrementLevel numbering, []) chfiAvailableOptions

incrementNumbering (numbering, item@ListFI{ .. }) = (incrementAtLevel numbering, item2)
  where
    item2 = item { lfiDescriptor = lfiDescriptor { iNumbering = numbering } }

incrementNumbering (numbering, item@Chapter{ chDescriptor, .. }) =
  (numbering1, item
                 { chDescriptor = chDescriptor { iNumbering = numbering }
                 , chItems = items2
                 })
  where
    numbering1 = incrementAtLevel numbering
    items2 = snd $ foldl numberItemInto (incrementLevel numbering, []) chItems

incrementNumbering (numbering, item@SimpleGroup{ sgDescriptor, .. }) =
  (numbering1, item
                 { sgDescriptor = sgDescriptor { iNumbering = numbering }
                 , sgLevel = getLevel numbering
                 , sgItems = items2
                 })
  where
    numbering1 = incrementAtLevel numbering
    items2 = snd $ foldl numberItemInto (incrementLevel numbering, []) sgItems

incrementNumbering (numbering, item@OptionalGroup{ ogDescriptor, .. }) =
  (numbering1, item
                 { ogDescriptor = ogDescriptor { iNumbering = numbering }
                 , ogLevel = getLevel numbering
                 , ogItems = items2
                 })
  where
    numbering1 = incrementAtLevel numbering
    items2 = snd $ foldl numberItemInto (incrementLevel numbering, []) ogItems

incrementNumbering (numbering, item@MultipleGroup{ mgDescriptor, .. }) =
  (numbering1, item
                 { mgDescriptor = mgDescriptor { iNumbering = numbering }
                 , mgLevel = getLevel numbering
                 , mgItems = items2
                 })
  where
    numbering1 = incrementAtLevel numbering
    items2 = snd $ foldl numberItemInto (incrementLevel numbering, []) mgItems

incrementNumbering (numbering, item) = (numbering, item)

prepareForm :: [FormItem] -> [FormItem]
prepareForm items = snd $ foldl numberItemInto (numbering0, []) items
  where
    numbering0 = Numbering (repeat 0) 0
