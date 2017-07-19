module FormEngine.Functionality where

import FormEngine.FormContext
import FormEngine.FormElement.FormElement

type ElemAction = FormElement -> FormContext -> IO ()

data Functionality = Functionality
  { funcImg :: String
  , funcAction :: ElemAction
  }

data ElemBehaviour = ElemBehaviour
  { focusAction :: Maybe ElemAction
  , blurAction :: Maybe ElemAction
  , clickAction :: Maybe ElemAction
  , detailsFunc :: Maybe Functionality
  }

emptyElemBehaviour :: ElemBehaviour
emptyElemBehaviour = ElemBehaviour
  { focusAction = Nothing
  , blurAction = Nothing
  , clickAction = Nothing
  , detailsFunc = Nothing
  }
