module FormEngine.Functionality where

import FormEngine.FormElement.FormElement
import FormEngine.FormContext

type ElemAction = FormElement -> FormContext -> IO ()

data Functionality = Functionality { funcImg :: String, funcAction :: ElemAction }

data ElemBehaviour = ElemBehaviour { focusAction :: Maybe ElemAction, blurAction :: Maybe ElemAction, detailsFunc :: Maybe Functionality }

