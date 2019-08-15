{-# LANGUAGE QuasiQuotes #-}

module SimpleIdClientMock where

import Data.Text                     (Text)
import qualified Data.Text as T
import NeatInterpolation


createChi :: Text -> IO Text
createChi name =
  pure [text|
    {
      "idCredPublic": "aefebac32aa3e12048a6ee7e6c44a3ac474b955ee9d9be711be1383ca110ea5841da28881631740de38878cdae9c53bd",
      "idCredPublicIP": "aefebac32aa3e12048a6ee7e6c44a3ac474b955ee9d9be711be1383ca110ea5841da28881631740de38878cdae9c53bd",
      "idCredSecret": "27b46d5b95b9e1ab1ec128b01f760e8ca622c7cd8da6867ba273cab7ce5d929b",
      "name": "$name"
    }
  |]
