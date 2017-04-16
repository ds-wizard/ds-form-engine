{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module FormEngine.FormElement.AutoComplete (
  autoCompleteHandler
) where

import Prelude
import Data.Foldable (foldlM)
import Data.Monoid ((<>))
import Data.List (isInfixOf)
import Data.Char (isSpace)

import FormEngine.JQuery as JQ
import FormEngine.FormItem
import FormEngine.FormContext
import FormEngine.FormElement.FormElement as Element
import FormEngine.FormElement.Identifiers
import FormEngine.FormElement.Updating

getMatches :: Maybe String -> [String] -> [String]
getMatches Nothing _ = []
getMatches (Just pattern) list = filter (\item -> pattern `isInfixOf` item) list

getAcExprFromText :: String -> Char -> Maybe String
getAcExprFromText str delim
  | delim `elem` str = case snd $ cutLastPart str "" delim of
    "" -> Nothing
    res -> getAcExprFromString res
  | otherwise = getAcExprFromString str

cutLastPart :: String -> String -> Char -> (String, String)
cutLastPart str1 str2 delim
  | null str1 = (str1, str2)
  | Prelude.last str1 == delim = (init str1, str2)
  | otherwise = cutLastPart (init str1) (Prelude.last str1 : str2) delim

getAcExprFromString :: String -> Maybe String
getAcExprFromString str = let str2 = strip str in
  if length str2 < 3 then Nothing
  else Just $ str2
    where
    strip :: String -> String
    strip s = dropWhile isSpace s

doAc :: String -> String -> Char -> String
doAc txt ac delim = let pre = fst $ cutLastPart txt "" delim in
  case pre of
    "" -> ac
    _ -> pre <> [delim] <> ac

autoCompleteHandler :: Char -> FormElement -> FormContext -> Handler
autoCompleteHandler delim element _ ev = do
  elem2 <- updateElementFromField element
  elemJq <- element2jq elem2
  left <- getLeft elemJq
  --top <- getTop elemJq
  acBoxJq <- selectById $ autoCompleteBoxId element
  keyCode <- getEvKeyCode ev
  if keyCode == "40" then do
    selJq <- findSelector "select" acBoxJq
    _ <- setFocus selJq
    return ()
  else do
    let matches = getMatches (getAcExprFromText (strValue elem2) delim) (iAutoComplete $ fiDescriptor $ formItem element)
    _ <- if not (null matches) then do
      _ <- setCss "left" (toPx left) acBoxJq
      setHtml "<select></select>" acBoxJq
      >>= setAttrInside "size" (if length matches > 10 then "10" else show $ length matches)
    --  >>= setCssInside "height" "0"
      >>= inside
        >>= makeOptions matches
      >>= parent
      >>= setKeypressHandler (copyFromAcHandler elemJq acBoxJq)
      >>= appearJq
     -- >>= animateInside
    else
      disappearJq acBoxJq
    return ()
  where
    makeOptions :: [String] -> JQuery -> IO JQuery
    makeOptions matches1 jq = foldlM (\jq1 i -> appendT ("<option>" <> i <> "</option>") jq1) jq matches1
    copyFromAcHandler :: JQuery -> JQuery -> Handler
    copyFromAcHandler elemJq acBoxJq ev1 = do
      keyCode <- getEvKeyCode ev1
      if keyCode == "13" then do
        preventDefault ev1
        txt <- getVal elemJq
        ac <- findSelector "select option:selected" acBoxJq >>= getText
        _ <- setVal (doAc txt ac delim) elemJq
        _ <- setFocus elemJq
        return ()
      else
        return ()
