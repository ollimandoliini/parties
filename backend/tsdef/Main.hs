{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Aeson.TypeScript.TH
  ( TypeScript (getTypeScriptDeclarations, getTypeScriptType),
    deriveTypeScript,
    formatTSDeclarations,
  )
import Data.Aeson.Types (defaultOptions)
import Data.Proxy (Proxy (..))
import Data.Time (UTCTime)
import Types

instance TypeScript UTCTime where
  getTypeScriptType _ = "string"

$(mconcat <$> traverse (deriveTypeScript defaultOptions) [''Event, ''Invite])

main =
  putStrLn $
    formatTSDeclarations $
      mconcat
        [ getTypeScriptDeclarations (Proxy :: Proxy Event),
          getTypeScriptDeclarations (Proxy :: Proxy Invite)
        ]