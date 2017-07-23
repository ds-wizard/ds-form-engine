{-# LANGUAGE OverloadedStrings #-}

module FormEngine.JQuery where

import Prelude hiding (last)
import Data.Monoid ((<>))
import Haste.DOM (Elem, ElemID, PropID, document)
import Haste.Foreign
import Haste

type Identifier = String
type ElementSelector = String
type JQuery = Elem
type EventType = String
type Handler = (JSAny -> IO ())

handlerCombinator :: [Handler] -> Handler
handlerCombinator handlers ev = mapM_ (\handler -> handler ev) handlers

manualFire :: JSAny
manualFire = toAny ("manual fire" :: JSString)

fireHandler :: Handler -> JQuery -> IO JQuery
fireHandler ev jq = do
  ev manualFire
  return jq

getEvKeyCode :: JSAny -> IO String
getEvKeyCode = ffi "(function (ev) { return ev.keyCode; })"

ready :: IO () -> IO ()
ready = ffi "(function (f) { jQuery(document).ready(f); })"

readyJq :: JQuery -> IO () -> IO JQuery
readyJq jq fun = do
  ffiReadyJq jq fun
  return jq
  where
    ffiReadyJq :: JQuery -> IO () -> IO ()
    ffiReadyJq = ffi "(function (jq, f) { jq.ready(f); })"

documentJq :: JQuery
documentJq = document

toJq :: Elem -> IO JQuery
toJq = ffi "(function (el) { return $(el); })"

parseHTML :: String -> IO JQuery
parseHTML el = let elJs = toJSString el in doFFI elJs
  where
    doFFI :: JSString -> IO JQuery
    doFFI = ffi "(function (elId) { return $.parseHTML(elId); })"

elemExists :: String -> IO Bool
elemExists el = let elJs = toJSString el in doFFI elJs
  where
    doFFI :: JSString -> IO Bool
    doFFI = ffi "(function (el) { return $(el).length > 0; })"

selectNoCheck :: String -> IO JQuery
selectNoCheck el = let elJs = toJSString el in doFFI elJs
  where
    doFFI :: JSString -> IO JQuery
    doFFI = ffi "(function (elId) { return $(elId); })"

select :: String -> IO JQuery
select el = let elJs = toJSString el in doFFI elJs
  where
    doFFI :: JSString -> IO JQuery
    doFFI = ffi "(function (elId) { var res = $(elId); if (res.length === 0) { console.warn('empty $ selection ' + elId); }; return res; })"

selectById :: String -> IO JQuery
selectById el = let elJs = toJSString el in doFFI elJs
  where
    doFFI :: JSString -> IO JQuery
    doFFI = ffi "(function (id) { return $('#' + id); })"

selectByIdentity :: String -> IO JQuery
selectByIdentity el = let elJs = toJSString el in doFFI elJs
  where
    doFFI :: JSString -> IO JQuery
    doFFI = ffi "(function (identity) { return $('[identity=\"' + identity + '\"]'); })"

selectByName :: String -> IO JQuery
selectByName el = let elJs = toJSString el in doFFI elJs
  where
    doFFI :: JSString -> IO JQuery
    doFFI = ffi "(function (name) { return $('[name=\"' + name + '\"]'); })"

selectRadio :: String -> IO JQuery
selectRadio name =  selectNoCheck $ "[name='" <> name <> "']:checked"

selectIn :: String -> JQuery -> IO JQuery
selectIn el context = let elJs = toJSString el in doFFI elJs context
  where
    doFFI :: JSString -> JQuery -> IO JQuery
    doFFI = ffi "(function (elId, context) { return $(elId, context); })"

findSelector :: ElemID -> JQuery -> IO JQuery
findSelector el jq = let elJs = toJSString el in doFFI elJs jq
  where
    doFFI :: JSString -> JQuery -> IO JQuery
    doFFI = ffi "(function (elJs, jq) { return jq.find(elJs); })"

jqLength :: JQuery -> IO Int
jqLength = ffi "(function (jq) { return jq.length; })"

getText :: JQuery -> IO String
getText = ffi "(function (domEl) { return domEl.text(); })"

setText :: String -> JQuery -> IO JQuery
setText txt jq = let txtJs = toJSString txt in doFFI txtJs jq
  where
    doFFI :: JSString -> JQuery -> IO JQuery
    doFFI = ffi "(function (txt, jq) { jq.text(txt); return jq; })"

getHtml :: JQuery -> IO String
getHtml = ffi "(function (domEl) { return domEl.html(); })"

setHtml :: String -> JQuery -> IO JQuery
setHtml html jq = let htmlJs = toJSString html in doFFI htmlJs jq
  where
    doFFI :: JSString -> JQuery -> IO JQuery
    doFFI = ffi "(function (html, jq) { jq.html(html); return jq; })"

getVal :: JQuery -> IO String
getVal = ffi "(function (jq) { return jq.val(); })"

setVal :: String -> JQuery -> IO JQuery
setVal val jq = let valJs = toJSString val in doFFI valJs jq
  where
    doFFI :: JSString -> JQuery -> IO JQuery
    doFFI = ffi "(function (val, jq) { jq.val(val).change(); return jq; })"

getAttr :: String -> JQuery -> IO JSString
getAttr key jq = let keyJs = toJSString key in doFFI keyJs jq
  where
    doFFI :: JSString -> JQuery -> IO JSString
    doFFI = ffi "(function (key, jq) { return jq.attr(key); })"

setAttr :: PropID -> String -> JQuery -> IO JQuery
setAttr k v jq = let kJs = toJSString k; vJs = toJSString v in doFFI kJs vJs jq
  where
    doFFI :: JSString -> JSString -> JQuery -> IO JQuery
    doFFI = ffi "(function (k, v, jq) { jq.attr(k, v); return jq; })"

setSelected :: JQuery -> IO JQuery
setSelected = ffi "(function (jq) { jq.prop('selected', true); return jq; })"

getCss :: String -> JQuery -> IO JSString
getCss key jq = let keyJs = toJSString key in doFFI keyJs jq
  where
    doFFI :: JSString -> JQuery -> IO JSString
    doFFI = ffi "(function (key, jq) { return jq.css(key); })"

setCss :: String -> String -> JQuery -> IO JQuery
setCss key val jq = let keyJs = toJSString key; valJs = toJSString val in doFFI keyJs valJs jq
  where
    doFFI :: JSString -> JSString -> JQuery -> IO JQuery
    doFFI = ffi "(function (key, val, jq) { jq.css(key, val); return jq; })"

hasClass :: String -> JQuery -> IO Bool
hasClass cls jq = let clsJs = toJSString cls in doFFI clsJs jq
  where
    doFFI :: JSString -> JQuery -> IO Bool
    doFFI = ffi "(function (cls, jq) { return jq.hasClass(cls); })"

addClass :: String -> JQuery -> IO JQuery
addClass cls jq = let clsJs = toJSString cls in doFFI clsJs jq
  where
    doFFI :: JSString -> JQuery -> IO JQuery
    doFFI = ffi "(function (cls, jq) { jq.addClass(cls); return jq; })"

removeClass :: String -> JQuery -> IO JQuery
removeClass cls jq = let clsJs = toJSString cls in doFFI clsJs jq
  where
    doFFI :: JSString -> JQuery -> IO JQuery
    doFFI = ffi "(function (cls, jq) { jq.removeClass(cls); return jq; })"

appendJq :: JQuery -> JQuery -> IO JQuery
appendJq = ffi "(function (jq, toJq) { return toJq.append(jq); })"

appendT :: String -> JQuery -> IO JQuery
appendT tag jq = let tagJs = toJSString tag in doFFI tagJs jq
  where
    doFFI :: JSString -> JQuery -> IO JQuery
    doFFI = ffi "(function (tag, jq) { return jq.append(tag); })"

removeJq :: JQuery -> IO JQuery
removeJq = ffi "(function (jq) { var p = jq.parent(); jq.remove(); return p; })"

parent :: JQuery -> IO JQuery
parent = ffi "(function (jq) { return jq.parent(); })"

children :: JQuery -> IO JQuery
children = ffi "(function (jq) { return jq.children(); })"

last :: JQuery -> IO JQuery
last = ffi "(function (jq) { return jq.last(); })"

prev :: JQuery -> IO JQuery
prev = ffi "(function (jq) { return jq.prev(); })"

text :: String -> JQuery -> IO JQuery
text = ffi "(function (txt, jq) { jq.text(txt); return jq; })"

getScrollTop :: JQuery -> IO Int
getScrollTop = ffi "(function (jq) { return jq.scrollTop(); })"

getHeight :: JQuery -> IO Int
getHeight = ffi "(function (jq) { return jq.height(); })"

setHeight :: Int -> JQuery -> IO JQuery
setHeight = ffi "(function (val, jq) { jq.height(val); return jq; })"

getWidth :: JQuery -> IO Int
getWidth = ffi "(function (jq) { return jq.width(); })"

setWidth :: Int -> JQuery -> IO JQuery
setWidth = ffi "(function (val, jq) { jq.width(val); return jq; })"

isRadioSelected :: String -> IO Bool
isRadioSelected name = do
  selected <- selectRadio name
  number <- jqLength selected
  return (number > 0)

getRadioValue :: String -> IO String
getRadioValue name = do
  selected <- selectRadio name
  getVal selected

-- Dumping ----------------------------------------------------------------

dumpjq :: JQuery -> IO JQuery
dumpjq jq = do
  ffiLog jq
  return jq
  where
    ffiLog :: JQuery -> IO ()
    ffiLog = ffi "(function (jq) { console.log(jq); })"

errorIO :: String -> IO ()
errorIO str = do
  ffiLog (toJSString str)
  return ()
  where
    ffiLog :: JSString -> IO ()
    ffiLog = ffi "(function (s) { console.error(s); })"

dumptIO :: String -> IO ()
dumptIO str = do
  ffiLog (toJSString str)
  return ()
  where
    ffiLog :: JSString -> IO ()
    ffiLog = ffi "(function (s) { console.log(s); })"

errorjq :: String -> JQuery -> IO JQuery
errorjq str jq = do
  ffiLog (toJSString str)
  return jq
  where
    ffiLog :: JSString -> IO ()
    ffiLog = ffi "(function (s) { console.error(s); })"

dumptjq :: String -> JQuery -> IO JQuery
dumptjq str jq = do
  ffiLog (toJSString str)
  return jq
  where
    ffiLog :: JSString -> IO ()
    ffiLog = ffi "(function (s) { console.log(s); })"

alertIO :: String -> IO ()
alertIO str = do
  ffiLog (toJSString str)
  return ()
  where
    ffiLog :: JSString -> IO ()
    ffiLog = ffi "(function (s) { alert(s); })"

-- Events ----------------------------------------------------------------

jqEvOff :: String -> JQuery -> IO JQuery
jqEvOff str jq = do
  ffiOff (toJSString str) jq
  return jq
  where
    ffiOff :: JSString -> JQuery -> IO ()
    ffiOff = ffi "(function (ev,jq) { jq.off(ev); })"

jqEvAllOff :: JQuery -> IO JQuery
jqEvAllOff = ffi "(function (jq) { jq.off(); })"

target :: JSAny -> IO JQuery
target = ffi "(function (ev) { return $(ev.target); })"

preventDefault :: JSAny -> IO ()
preventDefault = ffi "(function (ev) { ev.preventDefault(); })"

setFocus :: JQuery -> IO JQuery
setFocus jq = do
  ffifocus jq
  return jq
  where
    ffifocus :: JQuery -> IO ()
    ffifocus = ffi "(function (jq) { jq.focus(); })"

click :: JQuery -> IO JQuery
click jq = do
  ffiClick jq
  return jq
  where
    ffiClick :: JQuery -> IO ()
    ffiClick = ffi "(function (jq) { jq.click(); })"

enter :: JQuery -> IO JQuery
enter jq = do
  ffiEnter jq
  return jq
  where
    ffiEnter :: JQuery -> IO ()
    ffiEnter = ffi "(function (jq) { jq.mouseenter(); })"

blur :: JQuery -> IO JQuery
blur jq = do
  ffiBlur jq
  return jq
  where
    ffiBlur :: JQuery -> IO ()
    ffiBlur = ffi "(function (jq) { jq.blur(); })"

mouseleave :: JQuery -> IO JQuery
mouseleave jq = do
  ffiLeave jq
  return jq
  where
    ffiLeave :: JQuery -> IO ()
    ffiLeave = ffi "(function (jq) { jq.mouseleave(); })"

onBlur :: Handler -> JQuery -> IO JQuery
onBlur ev jq = do
  ffiBlur ev jq
  return jq
  where
    ffiBlur :: Handler -> JQuery -> IO ()
    ffiBlur = ffi "(function (ev, jq) { jq.blur(ev); })"

onKeyup :: Handler -> JQuery -> IO JQuery
onKeyup ev jq = do
  ffiKeyup ev jq
  return jq
  where
    ffiKeyup :: Handler -> JQuery -> IO ()
    ffiKeyup = ffi "(function (ev, jq) { jq.keyup(ev); })"

onKeypress :: Handler -> JQuery -> IO JQuery
onKeypress ev jq = do
  ffiKeypress ev jq
  return jq
  where
    ffiKeypress :: Handler -> JQuery -> IO ()
    ffiKeypress = ffi "(function (ev, jq) { jq.keypress(ev); })"

onClick :: Handler -> JQuery -> IO JQuery
onClick ev jq = do
  ffiClick ev jq
  return jq
  where
    ffiClick :: Handler -> JQuery -> IO ()
    ffiClick = ffi "(function (ev, jq) { jq.click(ev); })"

onChange :: Handler -> JQuery -> IO JQuery
onChange ev jq = do
  ffiChange ev jq
  return jq
  where
    ffiChange :: Handler -> JQuery -> IO ()
    ffiChange = ffi "(function (ev, jq) { jq.change(ev); })"

onMouseEnter :: Handler -> JQuery -> IO JQuery
onMouseEnter ev jq = do
  ffiMouseEnter ev jq
  return jq
  where
    ffiMouseEnter :: Handler -> JQuery -> IO ()
    ffiMouseEnter = ffi "(function (ev, jq) { jq.mouseenter(ev); })"

onMouseLeave :: Handler -> JQuery -> IO JQuery
onMouseLeave ev jq = do
  ffiMouseLeave ev jq
  return jq
  where
    ffiMouseLeave :: Handler -> JQuery -> IO ()
    ffiMouseLeave = ffi "(function (ev, jq) { jq.mouseleave(ev); })"

onFocus :: Handler -> JQuery -> IO JQuery
onFocus ev jq = do
  ffiOnFocus ev jq
  return jq
  where
    ffiOnFocus :: Handler -> JQuery -> IO ()
    ffiOnFocus = ffi "(function (ev, jq) { jq.focus(ev); })"

onScroll :: Handler -> JQuery -> IO JQuery
onScroll ev jq = do
  ffiOnScroll ev jq
  return jq
  where
    ffiOnScroll :: Handler -> JQuery -> IO ()
    ffiOnScroll = ffi "(function (ev, jq) { jq.scroll(ev); })"

onResize :: Handler -> JQuery -> IO JQuery
onResize ev jq = do
  ffiOnResize ev jq
  return jq
  where
    ffiOnResize :: Handler -> JQuery -> IO ()
    ffiOnResize = ffi "(function (ev, jq) { jq.resize(ev); })"

resize :: JQuery -> IO JQuery
resize jq = do
  ffiResize jq
  return jq
  where
    ffiResize :: JQuery -> IO ()
    ffiResize = ffi "(function (jq) { jq.resize(); })"

onLoad :: Handler -> JQuery -> IO JQuery
onLoad ev jq = do
  ffiOnLoad ev jq
  return jq
  where
    ffiOnLoad :: Handler -> JQuery -> IO ()
    ffiOnLoad = ffi "(function (ev, jq) { jq[0].addEventListener('load', ev); })"

-- Operations ------------------------------------------------------------

showJq :: JQuery -> IO JQuery
showJq = setCss "visibility" "visible"

hideJq :: JQuery -> IO JQuery
hideJq = setCss "visibility" "hidden"

appearJq :: JQuery -> IO JQuery
appearJq = setCss "display" "block"

disappearJq :: JQuery -> IO JQuery
disappearJq = setCss "display" "none"

disableJq :: JQuery -> IO JQuery
disableJq jq = do
  _ <- setCss "background-color" "#eee" jq
  setAttr "readonly" "true" jq

inside :: JQuery -> IO JQuery
inside jq = children jq >>= last

--insideButLast :: JQuery -> IO JQuery
--insideButLast = children >=> jq >=> prev

setTextInside :: String -> JQuery -> IO JQuery
setTextInside txt jq = inside jq >>= setText txt >>= parent

setAttrInside :: String -> String -> JQuery -> IO JQuery
setAttrInside attr val jq = inside jq >>= setAttr attr val >>= parent

setHtmlInside :: String -> JQuery -> IO JQuery
setHtmlInside val jq = inside jq >>= setHtml val >>= parent

setCssInside :: String -> String -> JQuery -> IO JQuery
setCssInside attr val jq = inside jq >>= setCss attr val >>= parent

addClassInside :: String -> JQuery -> IO JQuery
addClassInside cls jq = inside jq >>= addClass cls >>= parent

setChangeHandler :: Handler -> JQuery -> IO JQuery
setChangeHandler ev jq = inside jq >>= onChange ev >>= parent

setBlurHandler :: Handler -> JQuery -> IO JQuery
setBlurHandler ev jq = inside jq >>= onBlur ev >>= parent

setKeyupHandler :: Handler -> JQuery -> IO JQuery
setKeyupHandler ev jq = inside jq >>= onKeyup ev >>= parent

setKeypressHandler :: Handler -> JQuery -> IO JQuery
setKeypressHandler ev jq = inside jq >>= onKeypress ev >>= parent

setFocusHandler :: Handler -> JQuery -> IO JQuery
setFocusHandler ev jq = inside jq >>= onFocus ev >>= parent

setClickHandler :: Handler -> JQuery -> IO JQuery
setClickHandler ev jq = inside jq >>= onClick ev >>= parent

setMouseEnterHandler :: Handler -> JQuery -> IO JQuery
setMouseEnterHandler ev jq = inside jq >>= onMouseEnter ev >>= parent

setMouseLeaveHandler :: Handler -> JQuery -> IO JQuery
setMouseLeaveHandler ev jq = inside jq >>= onMouseLeave ev >>= parent

setScrollHandler :: Handler -> JQuery -> IO JQuery
setScrollHandler ev jq = inside jq >>= onScroll ev >>= parent

setLoadHandler :: Handler -> JQuery -> IO JQuery
setLoadHandler ev jq = inside jq >>= onLoad ev >>= parent

validateNotEmpty :: JQuery -> IO Bool
validateNotEmpty jq = do
  val <- getVal jq
  return $ not (null val)

isChecked :: JQuery -> IO Bool
isChecked = ffi "(function (jq) { return jq.prop('checked') === true; })"

isVisible :: JQuery -> IO Bool
isVisible = ffi "(function (jq) { return jq.is(':visible'); })"

getWindow :: IO JQuery
getWindow = ffi "(function () { return $(window); })"

getTop :: JQuery -> IO Int
getTop = ffi "(function (jq) { return jq.position().top; })"

getLeft :: JQuery -> IO Int
getLeft = ffi "(function (jq) { return jq.position().left; })"

toPx :: Int -> String
toPx val = show val ++ "px"

selectSVG :: String -> JQuery -> IO JQuery
selectSVG selector jq = let selectorJs = toJSString selector in doFFI selectorJs jq
  where
    doFFI :: JSString -> JQuery -> IO JQuery
    doFFI = ffi "(function (selector, jq) { if (jq[0].contentDocument !== null) { var res = $(selector, jq[0].contentDocument.documentElement); if (res.length === 0) { console.warn('empty $ selection ' + selector); }; return res; } else return jq; })"

-- AJAX ---------------------------------------------

serialize :: JQuery -> IO String
serialize = ffi "(function (jq) { return jq.serialize(); })"

ajaxSubmitForm :: JQuery -> (Maybe String -> IO ()) -> IO ()
ajaxSubmitForm = ffi "(function (jq, fn) { $.ajax({ type: 'POST', url: jq.attr('action'), data: jq.serialize(), success: fn, }); })"

-- Profiling -----------------------------------------

time :: String -> IO ()
time label = let labelJs = toJSString label in doFFI labelJs
  where
    doFFI :: JSString -> IO ()
    doFFI = ffi "(function (label) { console.time(label); })"

timeEnd :: String -> IO ()
timeEnd label = let labelJs = toJSString label in doFFI labelJs
  where
    doFFI :: JSString -> IO ()
    doFFI = ffi "(function (label) { console.timeEnd(label); })"

timeJq :: String -> JQuery -> IO JQuery
timeJq label jq = let labelJs = toJSString label in doFFI labelJs jq
  where
    doFFI :: JSString -> JQuery -> IO JQuery
    doFFI = ffi "(function (label, jq) { console.time(label); return jq; })"

timeEndJq :: String -> JQuery -> IO JQuery
timeEndJq label jq = let labelJs = toJSString label in doFFI labelJs jq
  where
    doFFI :: JSString -> JQuery -> IO JQuery
    doFFI = ffi "(function (label, jq) { console.timeEnd(label); return jq; })"

