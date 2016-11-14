{-# LANGUAGE OverloadedStrings, NamedFieldPuns, RecordWildCards #-}

module FormEngine.FormElement.Updating where

import Prelude
import           Data.Maybe (isJust, isNothing, catMaybes, mapMaybe)
import           Data.Monoid ((<>))
import           Text.Read (readMaybe)

import           FormEngine.JQuery hiding (parent)
import FormEngine.FormItem
import FormEngine.FormElement.FormElement as Element
import FormEngine.FormElement.Identifiers
import FormEngine.FormElement.Validation
import FormEngine.FormContext

element2jq :: FormElement -> IO JQuery
element2jq element = selectByName $ elementId element

identity2element :: String -> FormContext -> Maybe FormElement
identity2element identity1 context = identity2element' identity1 (allElems context)

identity2element' :: String -> [FormElement] -> Maybe FormElement
identity2element' _ [] = Nothing
identity2element' identity1 (element : rest) 
  | identity element == identity1 = Just element
  | otherwise = let i2 = identity2element' identity1 (Element.children element) in
      if isJust i2 then i2 else identity2element' identity1 rest 

updateElementFromField :: FormElement -> IO FormElement
updateElementFromField numberElem@NumberElem{..} = do
  val <- element2jq numberElem >>= getVal
  unit <- getRadioValue (nfiUnitId $ formItem numberElem)
  let unit' = if unit == "undefined" then "" else unit
  let e2 = updateNumberUnit (updateElementValue numberElem val) unit' 
  --_ <- dumpjq jq
  --dumptIO $ show unit
  --dumptIO $ show (neMaybeValue e2, neMaybeUnitValue e2)
  return e2
updateElementFromField element = do
  val <- element2jq element >>= getVal
  let e2 = updateElementValue element val 
  return e2

identity2elementUpdated :: String -> FormContext -> IO (Maybe FormElement)
identity2elementUpdated identity1 context = let maybeElement = identity2element identity1 context in
  case maybeElement of
    Nothing -> do 
      --dumptIO $ show $ allElems context 
      errorIO ("identity2elementUpdated: element with identity=" ++ show identity1 ++ " does not exist")
      return Nothing
    Just element -> do
      e2 <- updateElementFromField element
      return (Just e2)

updateValidityFlag :: FormElement -> FormContext -> Bool -> IO ()
updateValidityFlag element context valid = do
  --dumptIO " validated element -------------------------------"
  --dumptIO (show $ elementId element)
  flagPlaceJq <- select $ "#" <> flagPlaceId element
  oldValidityFlag <- selectIn ".validity-flag" flagPlaceJq
  _ <- removeJq oldValidityFlag
  if valid then 
    if Element.isMandatory element then do
      _ <- appendT (validImg context) flagPlaceJq
      return ()
    else 
      return ()
  else  
    if Element.isMandatory element then do
      _ <- appendT (invalidImg context) flagPlaceJq 
      return ()
    else
      return()

updateElementValue :: FormElement -> String -> FormElement
updateElementValue element@StringElem{} val = element { seValue = val }
updateElementValue element@TextElem{} val = element { teValue = val }
updateElementValue element@EmailElem{} val = element { eeValue = val }
updateElementValue element@NumberElem{} val = element { neMaybeValue = readMaybe val } 
updateElementValue element@ChoiceElem{ cheOptions, .. } val = element { cheOptions = updatedChoiceEs }
  where
  updatedChoiceEs = map updateChoiceE cheOptions
    where
    updateChoiceE :: OptionElement -> OptionElement
    updateChoiceE choiceE@SimpleOptionElem{ schi, .. } 
      | optionValue schi == val = choiceE{ scheSelected = True }
      | otherwise = choiceE{ scheSelected = False }
    updateChoiceE choiceE@DetailedOptionElem{ dchi, .. } 
      | optionValue dchi == val = choiceE{ dcheSelected = True }
      | otherwise = choiceE { dcheSelected = False }
updateElementValue element@ListElem{} val = element { leMaybeValue = if val == "" then Nothing else Just val }
updateElementValue element _ = element 

updateNumberUnit :: FormElement -> String -> FormElement
updateNumberUnit numberElem "" = numberElem{ neMaybeUnitValue = Nothing }
updateNumberUnit numberElem unit = numberElem{ neMaybeUnitValue = Just unit }

inputFieldUpdate :: FormElement -> FormContext -> IO ()
inputFieldUpdate element context = do
  elem2 <- updateElementFromField element
  updateValidityFlag elem2 context (validateElement elem2)

applyRule :: FormElement -> FormContext -> FormRule -> IO ()
applyRule NumberElem{..} context rule@(SumRule operandsIdents resultIdent) = do
  operandMaybeElems <- mapM (`identity2elementUpdated` context) operandsIdents
  if any isNothing operandMaybeElems then do
    errorIO ("invalid operand in " <> show rule)
    return ()
  else do
    let operandsVals = mapMaybe Element.neMaybeValue $ catMaybes operandMaybeElems
    resultJq <- selectByIdentity resultIdent
    _ <- setVal (show $ sum operandsVals) resultJq
    return ()
applyRule NumberElem{..} context (SumTBsRule operandsIdents resultIdent) = do
  operandMaybeElems <- mapM (`identity2elementUpdated` context) operandsIdents
  if any isNothing operandMaybeElems then 
    return ()
  else do
    let operandsVals = mapMaybe numberElem2TB $ catMaybes operandMaybeElems
    resultJq <- selectByIdentity resultIdent
    _ <- setVal (show $ sum operandsVals) resultJq
    return ()
applyRule _ _ (CopyValueRule operandIdent resultIdent) = do -- now works for simple field values only
  operandVal <- selectByIdentity operandIdent >>= getVal
  resultJq <- selectByIdentity resultIdent
  _ <- setVal operandVal resultJq
  return ()
applyRule element context (IntValueRule fn) = do
  e2 <- updateElementFromField element
  case neMaybeValue e2 of
    Nothing -> updateValidityFlag e2 context False
    Just value -> updateValidityFlag e2 context (fn value)
applyRule element _ ReadOnlyRule = do 
  _ <- element2jq element >>= disableJq 
  return ()
applyRule element _ rule = errorIO $ "Rule application did not unify: " <> show rule <> " @" <> show element  

applyRules :: FormElement -> FormContext -> IO () 
applyRules element context = mapM_ (applyRule element context) (rules element)

