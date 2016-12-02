{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module FormEngine.FormElement.Rendering (
  ElemAction
, ElemBehaviour(..)
, foldElements
, renderElement
) where

import Prelude
import Data.Monoid ((<>))
import Data.Foldable (foldlM)
import Data.Maybe (fromMaybe)
--import Haste.DOM

import FormEngine.JQuery as JQ
import FormEngine.FormItem 
import FormEngine.FormElement.FormElement as Element
import FormEngine.FormElement.Identifiers
import FormEngine.FormElement.Updating
import FormEngine.FormContext
import FormEngine.Functionality

setLongDescription :: FormElement -> IO ()
setLongDescription element = do
  paragraphJq <- select $ "#" ++ descSubpaneParagraphId element
  spanJq <- findSelector "span" paragraphJq 
  let maybeDesc = iLongDescription $ fiDescriptor $ formItem element
  case maybeDesc of
    Nothing -> return ()
    Just desc -> do
      _ <- setHtml desc spanJq
      _ <- appearJq paragraphJq
      return ()
  return ()

unsetLongDescription :: FormElement -> IO ()
unsetLongDescription element = do
  paragraphJq <- select $ "#" ++  descSubpaneParagraphId element
  _ <- disappearJq paragraphJq
  return ()

elementFocusHandler :: FormElement -> FormContext -> ElemBehaviour -> Handler
elementFocusHandler element context behaviour _ = do
  inputFieldUpdate element context  
  applyRules element context
  case focusAction behaviour of
    Nothing -> return ()
    Just action -> action element context
 
elementBlurHandler :: FormElement -> FormContext -> ElemBehaviour -> Handler
elementBlurHandler element context behaviour _ = do
  inputFieldUpdate element context
  applyRules element context
  case blurAction behaviour of
    Nothing -> return ()
    Just action -> action element context
-- handleItemMouseEnter :: FormItem -> FormItem -> Handler
-- handleItemMouseEnter = handleItemFocus

foldElements :: [FormElement] -> FormContext -> ElemBehaviour -> JQuery -> IO JQuery
foldElements elems context behaviour jq = foldlM (\jq1 e -> renderElement e context behaviour jq1) jq elems

renderElement :: FormElement -> FormContext -> ElemBehaviour -> JQuery -> IO JQuery
renderElement element@SimpleGroupElem{} context behaviour jq = renderSimpleGroup element context behaviour jq
renderElement element@OptionalGroupElem{} context behaviour jq = renderOptionalGroup element context behaviour jq
renderElement MultipleGroupElem{} _ _ jq = errorjq "MultipleGroupElement rendering not implemented yet" jq
renderElement element@StringElem{} context behaviour jq = renderStringElement element context behaviour jq
renderElement element@TextElem{} context behaviour jq = renderTextElement element context behaviour jq
renderElement element@EmailElem{} context behaviour jq = renderEmailElement element context behaviour jq
renderElement element@NumberElem{} context behaviour jq = renderNumberElement element context behaviour jq
renderElement element@ChoiceElem{} context behaviour jq = renderOptionElement element context behaviour jq
renderElement element@ListElem{} context behaviour jq = renderListElement element context behaviour jq
renderElement element@SaveButtonElem{} context _ jq = renderSaveButtonElement element context jq 
renderElement element@SubmitButtonElem{} context _ jq = renderSubmitButtonElement element context jq 
renderElement _ _ _ jq = errorjq "renderElement did not unify" jq 

renderLabel :: FormElement -> JQuery -> IO JQuery
renderLabel element jq = 
  case Element.maybeLabel element of 
    Nothing -> return jq
    Just label -> case Element.maybeLink element of
      Nothing -> appendT "<label>" jq >>= setTextInside label  
      Just link -> appendT ("<label class=\"link\" onclick=\"" <> link <> "\">") jq 
          >>= setTextInside label

renderHeading :: Maybe String -> Int -> JQuery -> IO JQuery
renderHeading Nothing _ jq = return jq  
renderHeading (Just label) lvl jq = appendT heading jq >>= setTextInside label
  where
  heading :: String
  heading = "<h" <> show lvl <> ">"

renderShortDesc :: FormElement -> JQuery -> IO JQuery
renderShortDesc element jq = let maybeDesc = iShortDescription $ fiDescriptor $ formItem element in
    case maybeDesc of
      Nothing -> return jq
      Just desc -> 
        appendT "<span class='short-desc'>" jq >>= setTextInside desc

renderInput :: IO JQuery -> FormElement -> FormContext -> ElemBehaviour ->  JQuery -> IO JQuery
renderInput elemIOJq element context behaviour jq = 
  appendT "<table>" jq 
    >>= setMouseEnterHandler (\_ -> setLongDescription element)
    >>= setMouseLeaveHandler (\_ -> unsetLongDescription element)
    >>= inside
      >>= appendT "<tbody>" 
      >>= inside
        >>= appendT "<tr>" 
        >>= inside
          >>= (case detailsFunc behaviour of
            Nothing -> return
            Just functionality -> renderQuestionDetails functionality)
          >>= renderLabelCell
          >>= renderElemCell
          >>= renderFlagCell
        >>= JQ.parent
      >>= JQ.parent
    >>= JQ.parent
    >>= renderShortDesc element 
    where
    renderQuestionDetails detFunc jq1 = appendT "<td>" jq1
        >>= inside
          >>= addClass "more-space"
          >>= appendT (funcImg detFunc)
          >>= setClickHandler (\_ -> funcAction detFunc element context)
        >>= JQ.parent
    renderLabelCell jq1 = appendT "<td class='labeltd'>" jq1
        >>= inside
          >>= addClass "more-space"
          >>= renderLabel element
        >>= JQ.parent
    renderElemCell jq1 = do
      elemJq <- elemIOJq
      appendT "<td>" jq1
        >>= inside
          >>= appendJq elemJq 
        >>= JQ.parent 
    renderFlagCell jq1 = appendT "<td>" jq1 >>= setAttrInside "id" (flagPlaceId element) 

renderStringElement :: FormElement -> FormContext -> ElemBehaviour -> JQuery -> IO JQuery
renderStringElement element context behaviour jq = 
  let
    elemIOJq = select "<input type='text'>"
      >>= setAttr "name" (elementId element)
      >>= setAttr "identity" (Element.identity element)
      >>= setAttr "value" (seValue element)
      >>= onMouseEnter (elementFocusHandler element context behaviour) 
      >>= onKeyup (elementFocusHandler element context behaviour) 
      >>= onBlur (elementBlurHandler element context behaviour) 
      >>= onMouseLeave (elementBlurHandler element context behaviour) 
  in renderInput elemIOJq element context behaviour jq 

renderTextElement :: FormElement -> FormContext -> ElemBehaviour -> JQuery -> IO JQuery
renderTextElement element context behaviour jq = 
  let
    elemIOJq = select "<textarea>"
      >>= setAttr "name" (elementId element)
      >>= setAttr "identity" (Element.identity element)
      >>= setAttr "value" (teValue element)
      >>= onMouseEnter (elementFocusHandler element context behaviour) 
      >>= onKeyup (elementFocusHandler element context behaviour) 
      >>= onBlur (elementBlurHandler element context behaviour) 
      >>= onMouseLeave (elementBlurHandler element context behaviour) 
  in renderInput elemIOJq element context behaviour jq 

renderEmailElement :: FormElement -> FormContext -> ElemBehaviour -> JQuery -> IO JQuery
renderEmailElement element context behaviour jq = 
  let
    elemIOJq = select "<input type='email'>"
      >>= setAttr "name" (elementId element)
      >>= setAttr "identity" (Element.identity element)
      >>= setAttr "value" (eeValue element)
      >>= onMouseEnter (elementFocusHandler element context behaviour) 
      >>= onKeyup (elementFocusHandler element context behaviour) 
      >>= onBlur (elementBlurHandler element context behaviour) 
      >>= onMouseLeave (elementBlurHandler element context behaviour) 
  in renderInput elemIOJq element context behaviour jq 

renderNumberElement :: FormElement -> FormContext -> ElemBehaviour -> JQuery -> IO JQuery
renderNumberElement element context behaviour jq = 
  let
    elemIOJq = select "<input type='number'>"
      >>= setAttr "id" (elementId element)
      >>= setAttr "name" (elementId element)
      >>= setAttr "identity" (Element.identity element)
      >>= setAttr "value"  (fromMaybe "" $ show <$> neMaybeValue element)
      >>= onMouseEnter (elementFocusHandler element context behaviour) 
      >>= onKeyup (elementFocusHandler element context behaviour) 
      >>= onBlur (elementBlurHandler element context behaviour) 
      >>= onMouseLeave (elementBlurHandler element context behaviour) 
      >>= appendT "&nbsp;" 
      >>= case nfiUnit (formItem element) of 
        NoUnit -> return 
        SingleUnit u -> appendT u 
        MultipleUnit units -> renderUnits units
      where
        renderUnits :: [String] -> JQuery -> IO JQuery
        renderUnits units jq1 = foldlM (flip renderUnit) jq1 units
          where
          renderUnit :: String -> JQuery -> IO JQuery
          renderUnit unit jq2 =
            appendT "<input type='radio'>" jq2 
            >>= setAttrInside "value" unit 
            >>= setAttrInside "name" (nfiUnitId $ nfi element)
            >>= setMouseEnterHandler (elementFocusHandler element context behaviour)
            >>= setClickHandler (elementFocusHandler element context behaviour)
            >>= setMouseLeaveHandler (elementBlurHandler element context behaviour)
            >>= case neMaybeUnitValue element of
              Nothing -> return 
              Just selectedOption -> if selectedOption == unit then setAttrInside "checked" "checked" else return
            >>= appendT "<label>" >>= setTextInside unit
            >>= appendT "&nbsp;&nbsp;"
  in renderInput elemIOJq element context behaviour jq 

renderListElement :: FormElement -> FormContext -> ElemBehaviour -> JQuery -> IO JQuery
renderListElement element context behaviour jq = 
  let
    selectIOJq = select "<select>" 
      >>= setAttr "name" (elementId element) 
      >>= setAttr "identity" (Element.identity element)
      >>= onBlur (elementFocusHandler element context behaviour) 
      >>= onChange (elementFocusHandler element context behaviour) 
      >>= onMouseLeave (elementBlurHandler element context behaviour) 
      >>= renderOptions
  in
    renderInput selectIOJq element context behaviour jq
    where
    renderOptions :: JQuery -> IO JQuery
    renderOptions jq1 = foldlM (flip renderOption) jq1 (lfiAvailableOptions (formItem element))
      where
      renderOption :: (String, String) -> JQuery -> IO JQuery
      renderOption (listVal, label) jq2 = 
        appendT "<option>" jq2 
        >>= setAttrInside "value" listVal 
        >>= setTextInside label
        >>= case leMaybeValue element of
          Nothing -> return 
          Just selectedOption -> if listVal == selectedOption then setAttrInside "selected" "selected" else return

choiceSwitchHandler :: FormElement -> OptionElement -> Handler
choiceSwitchHandler element optionElem _ = do 
  allPanes <- mapM selectOptionSection detailOptionElems
  mapM_ disappearJq allPanes
  case optionElem of
    SimpleOptionElem {} -> 
      return ()
    DetailedOptionElem {} -> do
      _ <- selectOptionSection optionElem >>= appearJq
      return ()
  where
    selectOptionSection :: OptionElement -> IO JQuery
    selectOptionSection oe = select $ "#" <> optionSectionId element oe
    detailOptionElems = Prelude.filter justDetailed (cheOptions element)
      where
        justDetailed :: OptionElement -> Bool
        justDetailed SimpleOptionElem{} = False
        justDetailed DetailedOptionElem{} = True
      
choiceValidateHandler :: FormElement -> FormContext -> Handler
choiceValidateHandler element context _ = do
  isSelected <- isRadioSelected $ radioName element
  updateValidityFlag element context isSelected
  -- Now a hack, needs to get the validity from the instances

renderRadio :: FormElement -> OptionElement -> FormContext -> JQuery -> IO JQuery
renderRadio element optionElem context jq = 
  --dumptIO (show $ optionElemValue optionElem)
   -- dumptIO (show $ choiceIisSelected choiceI)
  appendT "<input type='radio'>" jq
  >>= setAttrInside "id" (radioId element optionElem)
  >>= setAttrInside "name" (radioName element)
  >>= setAttrInside "identity" (Element.identity element)
  >>= setAttrInside "value" (optionElemValue optionElem)
  >>= (if optionElemIsSelected optionElem then setAttrInside "checked" "checked" else return)
  >>= setClickHandler (handlerCombinator (choiceSwitchHandler element optionElem) (choiceValidateHandler element context))
  >>= setMouseLeaveHandler (choiceValidateHandler element context)
  >>= appendT "<label>" 
  >>= setTextInside (optionElemValue optionElem)
   >>= appendT appendix
 where
   appendix :: String
   appendix = case optionElem of
     SimpleOptionElem {} -> ""
     DetailedOptionElem {} -> "▾"

renderOptionElement :: FormElement -> FormContext -> ElemBehaviour -> JQuery -> IO JQuery
renderOptionElement element context behaviour jq = 
  let elemIOJq = select "<div></div>" >>= renderButtons (cheOptions element) 
  in 
    renderInput elemIOJq element context behaviour jq
    >>= renderPanes (cheOptions element)
  where
    renderButtons :: [OptionElement] -> JQuery -> IO JQuery
    renderButtons optionElems jq1 = foldlM (flip renderButton) jq1 optionElems
      where
      renderButton :: OptionElement -> JQuery -> IO JQuery
      renderButton optionElem jq2 = 
        renderRadio element optionElem context jq2
        >>= (if optionElem == Prelude.last (cheOptions element) then return else appendT "<br>")
    renderPanes :: [OptionElement] -> JQuery -> IO JQuery
    renderPanes optionElems jq1 = foldlM (flip renderPane) jq1 optionElems
      where
      renderPane :: OptionElement -> JQuery -> IO JQuery
      renderPane SimpleOptionElem{} jq2 = return jq2
      renderPane optionElem@DetailedOptionElem{ dcheElements } jq2 = 
        appendT "<div>" jq2
        >>= setAttrInside "id" (optionSectionId element optionElem) >>= inside
          >>= disappearJq
          >>= foldElements dcheElements context behaviour 
        >>= JQ.parent

renderSimpleGroup :: FormElement -> FormContext -> ElemBehaviour -> JQuery -> IO JQuery
renderSimpleGroup element context behaviour  jq = let lvl = Element.level element in 
  --dumptIO $ fromMaybe "" (Element.maybeLabel element)
  appendT "<div class='simple-group'>" jq
  >>= setAttrInside "level" (show lvl)
  >>= (if lvl > 1 then addClassInside "framed" else return)   
  >>= inside
    >>= renderHeading (Element.maybeLabel element) lvl
    >>= renderShortDesc element
    >>= foldElements (sgeElements element) context behaviour
  >>= JQ.parent
 
renderOptionalGroup :: FormElement -> FormContext -> ElemBehaviour -> JQuery -> IO JQuery
renderOptionalGroup element context behaviour  jq = let lvl = Element.level element in 
  --dumptIO $ fromMaybe "" (Element.maybeLabel element)
  appendT "<div class='optional-group'>" jq
  >>= setAttrInside "level" (show lvl)
  >>= setMouseEnterHandler (\_ -> setLongDescription element)
  >>= setMouseLeaveHandler (\_ -> unsetLongDescription element)
  >>= inside
    >>= renderCheckbox 
    >>= appendT (if not $ null (ogeElements element) then "▾" else "")
    >>= renderShortDesc element 
    >>= renderOgContents
  >>= JQ.parent
  where
  renderCheckbox :: JQuery -> IO JQuery
  renderCheckbox jq1 =
    appendT "<input type='checkbox'>" jq1
    >>= setAttrInside "name" (elementId element)
    >>= (if ogeChecked element then setAttrInside "checked" "checked" else return)
    >>= setClickHandler handler
    >>= renderLabel element
    where
    handler ev = do
      sectionJq <- select $ "#" <>  checkboxId element
      checkBox <- target ev
      checked <- isChecked checkBox
      _ <- if checked then 
        appearJq sectionJq
        --tinkerDiagramForItem chapter item 
      else 
        disappearJq sectionJq
        --tinkerDiagramForItemBlur chapter item 
      return ()
  renderOgContents :: JQuery -> IO JQuery
  renderOgContents jq1 = 
    case ogeElements element of
      [] -> return jq1
      _ ->  
        appendT "<div class='optional-section'>" jq1 >>= setAttrInside "id" (checkboxId element)
        >>= inside
          >>= foldElements (ogeElements element) context behaviour
        >>= JQ.parent

-- renderMultipleGroup :: FormItem -> FormItem -> FIDescriptor -> Int -> [FormItem] -> Maybe FormData -> JQuery -> IO JQuery
-- renderMultipleGroup chapter item descriptor level mgItems maybeFormData jq = let maybeLabel = iLabel descriptor in
--   appendT "<div class='multiple-group'>" jq 
--   >>= (if level > 1 then addClassInside "framed" else return) 
--   >>= setAttrInside "level" (show $ fiLevel item)
--   >>= inside
--     >>= renderHeading maybeLabel level
--     >>= renderShortDesc element
--     >>= renderMgLine mgItems 0
--     >>= renderAddButton 
--    >>= JQ.parent
--   where
--   renderMgLine :: [FormItem] -> Integer -> JQuery -> IO JQuery
--   renderMgLine mgItems number jq = 
--     appendT "<table>" jq >>= inside -- MG item holder
--       >>= appendT "<tbody>" >>= inside
--         >>= appendT "<tr>" >>= inside
--           >>= appendT "<td>" >>= inside
--             >>= appendT "<div class='multiple-section'>"  
--             >>= inside
--               >>= foldElements chapter mgItems maybeFormData
--             >>= JQ.parent
--           >>= JQ.parent
--           >>= (if number > 0 then renderRemoveButton else return) 
--         >>= JQ.parent
--       >>= JQ.parent
--     >>= JQ.parent
--     where
--     renderRemoveButton :: JQuery -> IO JQuery
--     renderRemoveButton jq = 
--       appendT "<td style='vertical-align: middle;'>" jq >>= inside
--         >>= appendT "<img class='button-add' src='static/img/remove.png'/>"
--         >>= setClickHandler removingHandler  
--       >>= JQ.parent
--       where
--       removingHandler :: Handler
--       removingHandler ev = do
--         minusButtonJq <- target ev
--         tableJq <- JQ.parent minusButtonJq >>= JQ.parent >>= JQ.parent >>= JQ.parent -- img -> td -> tr -> tbody -> table
--         remove tableJq
--         return ()
--   renderAddButton :: JQuery -> IO JQuery
--   renderAddButton jq = 
--     appendT "<img class='button-add' src='static/img/add.png'/>" jq
--     >>= setAttrInside "count" "0"
--     >>= setClickHandler addingHandler  
--     where
--     addingHandler :: Handler
--     addingHandler ev = do
--       plusButtonJq <- target ev
--       countStr <- getAttr "count" plusButtonJq
--       let countNum = read (unpack countStr) :: Integer
--       setAttr "count" (show $ countNum + 1) plusButtonJq
--       tableJq <- prev plusButtonJq
--       renderMgLine mgItems countNum tableJq 
--       return ()

renderSubmitButtonElement :: FormElement -> FormContext -> JQuery -> IO JQuery
renderSubmitButtonElement element _ jq = 
  appendT "<table style='margin-top: 10px'>" jq >>= inside
    >>= appendT "<tbody>" >>= inside
      >>= appendT "<tr>" >>= inside
        >>= appendT "<td class='labeltd more-space' style='text-align: center'>" >>= inside
          >>= appendT "<input type='button' class='submit'>"
          -- >>= setClickHandler submitHandler
          >>= setAttrInside "value" (fromMaybe "Submit" (Element.maybeLabel element))
        >>= JQ.parent
      >>= JQ.parent
    >>= JQ.parent
  >>= JQ.parent
  >>= renderShortDesc element
  --  where
  --  submitHandler :: Handler
  --  submitHandler ev = 

renderSaveButtonElement :: FormElement -> FormContext -> JQuery -> IO JQuery
renderSaveButtonElement element _ jq = 
  appendT "<table style='margin-top: 10px'>" jq >>= inside
    >>= appendT "<tbody>" >>= inside
      >>= appendT "<tr>" >>= inside
        >>= appendT "<td class='labeltd more-space' style='text-align: center'>" >>= inside
          >>= appendT "<input type='submit'>"
          >>= setAttrInside "value" (fromMaybe "Save" (Element.maybeLabel element)) 
        >>= JQ.parent 
      >>= JQ.parent
    >>= JQ.parent
  >>= JQ.parent
  >>= renderShortDesc element
