{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module IntelliMonad.Config where

import Data.Yaml
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | LLM backend selection. Parsed case-insensitively from the
-- @backend@ field of @intellimonad-config.yaml@.
data BackendType = OpenAI | Anthropic | Gemini
  deriving (Show, Eq, Generic)

instance FromJSON BackendType where
  parseJSON = withText "BackendType" $ \t -> case T.toLower t of
    "openai" -> pure OpenAI
    "anthropic" -> pure Anthropic
    "gemini" -> pure Gemini
    other -> fail $ "Unknown backend type: " <> T.unpack other <> ". Must be one of: openai, anthropic, gemini"

instance ToJSON BackendType where
  toJSON OpenAI = String "openai"
  toJSON Anthropic = String "anthropic"
  toJSON Gemini = String "gemini"

-- | Top-level configuration, read from @intellimonad-config.yaml@
-- in the working directory.
data Config = Config
  { apiKey :: Text             -- ^ Bearer key sent to the backend.
  , endpoint :: Text           -- ^ Base URL of the OpenAI-compatible API.
  , model :: Text              -- ^ Model name (e.g. @gpt-4o-mini@).
  , backend :: Maybe BackendType  -- ^ Optional, defaults to 'OpenAI'.
  , useStreaming :: Maybe Bool    -- ^ Optional, defaults to 'True'.
  } deriving (Show, Generic)

instance FromJSON Config

-- | Get whether to use streaming (defaults to True if not specified)
getUseStreaming :: Config -> Bool
getUseStreaming cfg = case useStreaming cfg of
  Just val -> val
  Nothing -> True  -- Default to streaming enabled

-- | Read and decode @intellimonad-config.yaml@; errors hard if the
-- file is missing or malformed.
readConfig :: IO Config
readConfig = do
  config <- decodeFileEither "intellimonad-config.yaml"
  case config of
    Left err -> error $ "Error reading config file: " ++ show err
    Right cfg -> return cfg
