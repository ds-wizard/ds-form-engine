-----------------------------------------------------------------------------
--
-- Module      :  Builder
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- | Monad and Monoid instances for a builder that hang DOM elements from the
-- current parent element. It uses Haste.DOM from the haste-compiler
--
-----------------------------------------------------------------------------
{-# LANGUAGE CPP, TypeSynonymInstances, FlexibleInstances,
  OverloadedStrings, DeriveDataTypeable, UndecidableInstances,
  OverlappingInstances #-}

module FormEngine.Perch where

import Data.Typeable
import Haste
import Haste.DOM hiding (Attribute, attr)
import Haste.Events
import Haste.Foreign
import Unsafe.Coerce
import Data.String
import Control.Monad.IO.Class

newtype PerchM a = Perch
  { build :: Elem -> IO Elem
  } deriving (Typeable)

type Perch = PerchM ()

instance Monoid (PerchM a) where
  mappend mx my =
    Perch $
    \e -> do
      build mx e
      build my e
      return e
  mempty = Perch return

instance Functor PerchM

instance Applicative PerchM

instance Monad PerchM where
  (>>) x = mappend (unsafeCoerce x)
  (>>=) = error "bind (>>=) invocation in the Perch monad creating DOM elements"
  return = mempty

instance MonadIO PerchM where
  liftIO mx = Perch $ \e -> mx >> return e

instance IsString Perch where
  fromString = toElem

class ToElem a  where
  toElem :: a -> Perch

instance ToElem String where
  toElem s =
    Perch $
    \e -> do
      e' <- newTextElem s
      appendChild e e'
      return e'

instance Show a =>
         ToElem a where
  toElem = toElem . show

instance ToElem (PerchM a) where
  toElem = unsafeCoerce

nelem :: String -> Perch
nelem s =
  Perch $
  \e -> do
    e' <- newElem s
    appendChild e e'
    return e'

child
  :: ToElem a
  => Perch -> a -> Perch
child me ch =
  Perch $
  \e' -> do
    e <- build me e'
    build (toElem ch) e

setHtml :: Perch -> String -> Perch
setHtml me text =
  Perch $
  \e' -> do
    e <- build me e'
    inner e text
    return e'
  where
    inner :: Elem -> String -> IO ()
    inner e = setProp e "innerHTML"

-- | create an element and add a Haste event handler to it.
--addEvent :: Perch -> Event IO a -> a -> Perch
addEvent be event action =
  Perch $
  \e -> do
    e' <- build be e
    let atr1 = fromJSStr $ eventName event
    has1 <- getAttr e' atr1
    case has1 of
      "true" -> return e'
      _ -> do
        onEvent e' event action
        setAttr e' atr1 "true"
        return e'

--listen :: JSType event => Elem -> event -> a -> IO Bool
--listen e event f= jsSetCB e (toJSString event) (mkCallback $! f)
--
--
--foreign import ccall jsSetCB :: Elem -> JSString -> JSFun a -> IO Bool
-- Leaf DOM nodes
--
area :: Perch 
area = nelem "area"

base :: Perch 
base = nelem "base"

br :: Perch
br = nelem "br"

col :: Perch
col = nelem "col"

embed :: Perch
embed = nelem "embed"

hr :: Perch
hr = nelem "hr"

img :: Perch
img = nelem "img"

input :: Perch
input = nelem "input"

keygen :: Perch
keygen = nelem "keygen"

link :: Perch
link = nelem "link"

menuitem :: Perch
menuitem = nelem "menuitem"

meta :: Perch
meta = nelem "meta"

param :: Perch
param = nelem "param"

source :: Perch
source = nelem "source"

track :: Perch
track = nelem "track"

wbr :: Perch
wbr = nelem "wbr"

-- Parent DOM nodes


a :: ToElem a => a -> Perch
a cont = nelem "a" `child` cont

abbr :: ToElem a => a -> Perch
abbr cont = nelem "abbr" `child` cont

address :: ToElem a => a -> Perch
address cont = nelem "address" `child` cont

article :: ToElem a => a -> Perch
article cont = nelem "article" `child` cont

aside :: ToElem a => a -> Perch
aside cont = nelem "aside" `child` cont

audio :: ToElem a => a -> Perch
audio cont = nelem "audio" `child` cont

b :: ToElem a => a -> Perch
b cont = nelem "b" `child` cont

bdo :: ToElem a => a -> Perch
bdo cont = nelem "bdo" `child` cont

blockquote :: ToElem a => a -> Perch
blockquote cont = nelem "blockquote" `child` cont

body :: ToElem a => a -> Perch
body cont = nelem "body" `child` cont

button :: ToElem a => a -> Perch
button cont = nelem "button" `child` cont

canvas :: ToElem a => a -> Perch
canvas cont = nelem "canvas" `child` cont

caption :: ToElem a => a -> Perch
caption cont = nelem "caption" `child` cont

cite :: ToElem a => a -> Perch
cite cont = nelem "cite" `child` cont

code :: ToElem a => a -> Perch
code cont = nelem "code" `child` cont

colgroup :: ToElem a => a -> Perch
colgroup cont = nelem "colgroup" `child` cont

command :: ToElem a => a -> Perch
command cont = nelem "command" `child` cont

datalist :: ToElem a => a -> Perch
datalist cont = nelem "datalist" `child` cont

dd :: ToElem a => a -> Perch
dd cont = nelem "dd" `child` cont

del :: ToElem a => a -> Perch
del cont = nelem "del" `child` cont

details :: ToElem a => a -> Perch
details cont = nelem "details" `child` cont

dfn :: ToElem a => a -> Perch
dfn cont = nelem "dfn" `child` cont

div :: ToElem a => a -> Perch
div cont = nelem "div" `child` cont

dl :: ToElem a => a -> Perch
dl cont = nelem "dl" `child` cont 

dt :: ToElem a => a -> Perch
dt cont = nelem "dt" `child` cont

em :: ToElem a => a -> Perch
em cont = nelem "em" `child` cont

fieldset :: ToElem a => a -> Perch
fieldset cont = nelem "fieldset" `child` cont

figcaption :: ToElem a => a -> Perch
figcaption cont = nelem "figcaption" `child` cont

figure :: ToElem a => a -> Perch
figure cont = nelem "figure" `child` cont

footer :: ToElem a => a -> Perch
footer cont = nelem "footer" `child` cont

form :: ToElem a => a -> Perch
form cont = nelem "form" `child` cont

h1 :: ToElem a => a -> Perch
h1 cont = nelem "h1" `child` cont

h2 :: ToElem a => a -> Perch
h2 cont = nelem "h2" `child` cont

h3 :: ToElem a => a -> Perch
h3 cont = nelem "h3" `child` cont

h4 :: ToElem a => a -> Perch
h4 cont = nelem "h4" `child` cont

h5 :: ToElem a => a -> Perch
h5 cont = nelem "h5" `child` cont

h6 :: ToElem a => a -> Perch
h6 cont = nelem "h6" `child` cont

head :: ToElem a => a -> Perch
head cont = nelem "head" `child` cont

header :: ToElem a => a -> Perch
header cont = nelem "header" `child` cont

hgroup :: ToElem a => a -> Perch
hgroup cont = nelem "hgroup" `child` cont

html :: ToElem a => a -> Perch
html cont = nelem "html" `child` cont

i :: ToElem a => a -> Perch
i cont = nelem "i" `child` cont

iframe :: ToElem a => a -> Perch
iframe cont = nelem "iframe" `child` cont

ins :: ToElem a => a -> Perch
ins cont = nelem "ins" `child` cont

kbd :: ToElem a => a -> Perch
kbd cont = nelem "kbd" `child` cont

label :: ToElem a => a -> Perch
label cont = nelem "label" `child` cont

legend :: ToElem a => a -> Perch
legend cont = nelem "legend" `child` cont

li :: ToElem a => a -> Perch
li cont = nelem "li" `child` cont

map :: ToElem a => a -> Perch
map cont = nelem "map" `child` cont

mark :: ToElem a => a -> Perch
mark cont = nelem "mark" `child` cont

menu :: ToElem a => a -> Perch
menu cont = nelem "menu" `child` cont

meter :: ToElem a => a -> Perch
meter cont = nelem "meter" `child` cont

nav :: ToElem a => a -> Perch
nav cont = nelem "nav" `child` cont

noscript :: ToElem a => a -> Perch
noscript cont = nelem "noscript" `child` cont

object :: ToElem a => a -> Perch
object cont = nelem "object" `child` cont

ol :: ToElem a => a -> Perch
ol cont = nelem "ol" `child` cont

optgroup :: ToElem a => a -> Perch
optgroup cont = nelem "optgroup" `child` cont

option :: ToElem a => a -> Perch
option cont = nelem "option" `child` cont

output :: ToElem a => a -> Perch
output cont = nelem "output" `child` cont

p :: ToElem a => a -> Perch
p cont = nelem "p" `child` cont

pre :: ToElem a => a -> Perch
pre cont = nelem "pre" `child` cont

progress :: ToElem a => a -> Perch
progress cont = nelem "progress" `child` cont

q :: ToElem a => a -> Perch
q cont = nelem "q" `child` cont

rp :: ToElem a => a -> Perch
rp cont = nelem "rp" `child` cont

rt :: ToElem a => a -> Perch
rt cont = nelem "rt" `child` cont

ruby :: ToElem a => a -> Perch
ruby cont = nelem "ruby" `child` cont

samp :: ToElem a => a -> Perch
samp cont = nelem "samp" `child` cont

script :: ToElem a => a -> Perch
script cont = nelem "script" `child` cont

section :: ToElem a => a -> Perch
section cont = nelem "section" `child` cont

select :: ToElem a => a -> Perch
select cont = nelem "select" `child` cont

small :: ToElem a => a -> Perch
small cont = nelem "small" `child` cont

span :: ToElem a => a -> Perch
span cont = nelem "span" `child` cont

strong :: ToElem a => a -> Perch
strong cont = nelem "strong" `child` cont

summary :: ToElem a => a -> Perch
summary cont = nelem "summary" `child` cont

sup :: ToElem a => a -> Perch
sup cont = nelem "sup" `child` cont

table :: ToElem a => a -> Perch
table cont = nelem "table" `child` cont

tbody :: ToElem a => a -> Perch
tbody cont = nelem "tbody" `child` cont

td :: ToElem a => a -> Perch
td cont = nelem "td" `child` cont

textarea :: ToElem a => a -> Perch
textarea cont = nelem "textarea" `child` cont

tfoot :: ToElem a => a -> Perch
tfoot cont = nelem "tfoot" `child` cont

th :: ToElem a => a -> Perch
th cont = nelem "th" `child` cont

thead :: ToElem a => a -> Perch
thead cont = nelem "thead" `child` cont

time :: ToElem a => a -> Perch
time cont = nelem "time" `child` cont

title :: ToElem a => a -> Perch
title cont = nelem "title" `child` cont

tr :: ToElem a => a -> Perch
tr cont = nelem "tr" `child` cont

ul :: ToElem a => a -> Perch
ul cont = nelem "ul" `child` cont

var :: ToElem a => a -> Perch
var cont = nelem "var" `child` cont

video :: ToElem a => a -> Perch
video cont = nelem "video" `child` cont

ctag :: ToElem a => String -> a -> Perch
ctag tag cont = nelem tag `child` cont

noHtml :: Perch 
noHtml = mempty :: Perch

---------------- Attributes

type Attribute = (String, String)

attr :: Perch -> Attribute -> Perch
attr tag (n, v) =
  Perch $
  \e' -> do
    e <- build tag e'
    setAttr e n v
    return e

class Attributable h  where
  (!) :: h -> Attribute -> h

instance ToElem a =>
         Attributable (a -> Perch) where
  (!) pe atrib = \e -> pe e `attr` atrib

instance Attributable Perch where
  (!) = attr

atr :: String -> String -> Attribute
atr n v = (n, v)

style :: String -> Attribute
style = atr "style"

id :: String -> Attribute
id = atr "id"

width :: String -> Attribute
width = atr "width"

height :: String -> Attribute
height = atr "height"

href :: String -> Attribute
href = atr "href"

src :: String -> Attribute
src = atr "src"

---------------- DOM Tree navigation and edition
-- | return the current node
this :: Perch
this = Perch $ \e -> return e

-- | goes to the parent node of the first and execute the second
goParent :: Perch -> Perch -> Perch
goParent pe pe' =
  Perch $
  \e' -> do
    e <- build pe e'
    p1 <- parent e
    build pe' p1

-- | delete the current node. Return the parent
delete :: Perch
delete =
  Perch $
  \e -> do
    p1 <- parent e
    removeChild e p1
    return p1

-- | delete the children of the current node.
clear :: Perch
clear = Perch $ \e -> clearChildren e >> return e

-- | replace the current node with a new one
outer :: Perch -> Perch -> Perch
outer olde newe =
  Perch $
  \e'' -> do
    e <- build olde e''
    e' <- build newe e''
    replace e e'

replace :: Elem -> Elem -> IO Elem
replace = ffi "(function(e,e1){var par=  e.parentNode;par.replaceChild(e1,e);return e1;})"

parent :: Elem -> IO Elem
parent = ffi "(function(e){return e.parentNode;})"

getBody :: IO Elem
getBody = ffi "(function(){return document.body;})"

getWindow :: IO Elem
getWindow = ffi "(function(){return window;})"

-- ! JQuery-like DOM manipulation: using a selector for querySelectorAll,
-- it apply the Perch DOM manipulation of the second parameter for each of the matches
--
-- Example
--
-- > main= do
-- >  body <- getBody
-- >  (flip build) body $ pre $ do
-- >      div ! atr "class" "modify" $ "click"
-- >      div $ "not changed"
-- >      div ! atr "class" "modify" $ "here"
-- >
-- >      addEvent this OnClick $ \_ _ -> do
-- >          forElems' ".modify" $  this ! style "color:red"
forElems' :: String -> Perch -> IO ()
forElems' for doit = do
  build (forElems for doit) undefined
  return ()

-- | a more declarative synmonym of `forElems'`
withElems' = forElems'

-- ! JQuery-like DOM manipulation: using a selector for querySelectorAll,
-- it apply the Perch DOM manipulation of the second parameter for each of the matches
forElems :: String -> Perch -> Perch
forElems selectors dosomething =
  Perch $
  \e -> do
    es <- queryAll selectors
    mapM_ (build dosomething) es
    return e
  where
    queryAll :: String -> IO [Elem]
    queryAll = ffi "(function(sel){return document.querySelectorAll(sel);})"

-- | a more declarative synmonym of `forElems`
withElems = forElems
