{-# LANGUAGE OverloadedStrings, CPP #-}

module FormEngine.AutoComplete
  ( getMatches
  ) where

#ifdef __HASTE__
import Data.List (isInfixOf)
type Text = String
#else
import Data.Text.Lazy (Text)
import Data.List (isInfixOf)
#endif


getMatches :: Text -> [Text] -> [Text]
getMatches pattern list = filter (\item -> pattern `isInfixOf` item) list
