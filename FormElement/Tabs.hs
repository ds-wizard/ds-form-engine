{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module FormEngine.FormElement.Tabs (
    renderTabGroup,
    toTab,
    colorizeTabs,
    selectTab,
    ) where

import           Prelude
import           Data.Monoid ((<>))
import           Data.Foldable (foldlM)

import           FormEngine.JQuery as JQ
import           FormEngine.FormElement.FormElement
import           FormEngine.FormElement.Identifiers

tab2tabJq :: FormElement -> IO JQuery
tab2tabJq tab = select $ "#" <> tabId tab

tab2PaneJq :: FormElement -> IO JQuery
tab2PaneJq tab = select $ "#" <> paneId tab
--
-- setNotValidNotCurrent :: Tab -> IO JQuery
-- setNotValidNotCurrent tab = tab2tabJq tab >>=
--                             addClass "notvalid-notcurrent" >>=
--                             removeClass "notvalid-current" >>=
--                             removeClass "valid-notcurrent" >>=
--                             removeClass "valid-current"
--
-- setNotValidCurrent :: Tab -> IO JQuery
-- setNotValidCurrent tab = tab2tabJq tab >>=
--                          removeClass "notvalid-notcurrent" >>=
--                          addClass "notvalid-current" >>=
--                          removeClass "valid-notcurrent" >>=
--                          removeClass "valid-current"
--
-- setValidNotCurrent :: Tab -> IO JQuery
-- setValidNotCurrent tab = tab2tabJq tab >>=
--                          removeClass "notvalid-notcurrent" >>=
--                          removeClass "notvalid-current" >>=
--                          addClass "valid-notcurrent" >>=
--                          removeClass "valid-current"
--
-- setValidCurrent :: Tab -> IO JQuery
-- setValidCurrent tab = tab2tabJq tab >>=
--                       removeClass "notvalid-notcurrent" >>=
--                       removeClass "notvalid-current" >>=
--                       removeClass "valid-notcurrent" >>=
--                       addClass "valid-current"
--
colorizeTabs :: FormElement -> [FormElement] -> IO ()
colorizeTabs activeTab = mapM_ applyColour
  where
    applyColour :: FormElement -> IO JQuery
    applyColour tab
      | tab /= activeTab = tab2tabJq tab >>= removeClass "current" >>= addClass "notcurrent"
      | otherwise = tab2tabJq tab >>= removeClass "notcurrent" >>= addClass "current"

toTab :: FormElement -> [FormElement] -> IO ()
toTab tab tabGroup = do
   activePaneJq <- tab2PaneJq tab
   panesJq <- mapM tab2PaneJq tabGroup
   colorizeTabs tab tabGroup
   mapM_ disappearJq panesJq
   _ <- appearJq activePaneJq
   return ()

--refreshHandlers tabGroup
--  where
--  refreshHandlers :: [Tab] -> IO ()
--  refreshHandlers new[Tab] = mapM_ refreshHandler new[Tab]
--    where
--    refreshHandler :: Tab -> IO ()
--    refreshHandler tab = do
--      tabJq <- tab2tabJq tab
--      onClick (tabHandler tab new[Tab]) tabJq
--      return ()

tabHandler :: FormElement -> [FormElement] -> Handler
tabHandler tab tabGroup _ = toTab tab tabGroup

renderTabGroup :: [FormElement] -> [IO JQuery] -> JQuery -> IO JQuery
renderTabGroup tabs panesIOJqs jq =
  appendT "<ul>" jq
  >>= inside
    >>= addClass "nav"
    >>= makeHeadings
  >>= JQ.parent
  >>= makeStripe
  >>= makePanes
  where
    makeHeadings :: JQuery -> IO JQuery
    makeHeadings jq2 = foldlM (flip makeHeading) jq2 tabs
      where
        makeHeading :: FormElement -> JQuery -> IO JQuery
        makeHeading tab jq3 =
          appendT "<li>" jq3
          >>= setAttrInside "id" (tabId tab)
          >>= inside
            >>= appendT "<a>"
            >>= onClick (tabHandler tab tabs)
            >>= setTextInside (show $ tabName tab)
         -- >>= click
          >>= JQ.parent
    makeStripe :: JQuery -> IO JQuery
    makeStripe = appendT "<div class='stripe stripe-thin'>"
    makePanes :: JQuery -> IO JQuery
    makePanes jq4 = foldlM (flip foldPane) jq4 (zip tabs panesIOJqs)
      where
        foldPane :: (FormElement, IO JQuery) -> JQuery -> IO JQuery
        foldPane (tab, paneIOJq) jq5 = do
                         -- time "foldPane"
                          paneJq <- paneIOJq
                          res <- appendJq paneJq jq5
                            >>= setAttrInside "id" (paneId tab)
                            >>= setCssInside "display" "none"
                            >>= setAttrInside "class" "inside-bordered"
                         -- timeEnd "foldPane"
                          return res

selectTab :: Int -> [FormElement] -> IO JQuery
selectTab n tabs = do
  tabJq <- tab2tabJq (tabs !! n)
  click tabJq
