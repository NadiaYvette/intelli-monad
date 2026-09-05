{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module IntelliMonad.CustomInstructions where

import qualified Data.Aeson as A
import Data.Proxy
import GHC.Generics
import IntelliMonad.Types

-- | No custom instructions by default.
defaultCustomInstructions :: [CustomInstructionProxy]
defaultCustomInstructions = []

-- | Sample tool: echoes a number back (exercises the tool loop).
data ValidateNumber = ValidateNumber
  { number :: Double
  }
  deriving (Eq, Show, Generic, JSONSchema, A.FromJSON, A.ToJSON)

instance HasFunctionObject ValidateNumber where
  getFunctionName = "output_number"
  getFunctionDescription = "validate input number"
  getFieldDescription "number" = "A number that system outputs."

instance Tool ValidateNumber where
  data Output ValidateNumber = ValidateNumberOutput
    { code :: Int,
      stdout :: String,
      stderr :: String
    }
    deriving (Eq, Show, Generic, A.FromJSON, A.ToJSON)
  toolExec _ = do
    return $ ValidateNumberOutput 0 "" ""

-- | Sample custom instruction: prompts the model to compute an
-- answer and call the 'ValidateNumber' tool with it.
data Math = Math

instance CustomInstruction Math where
  customHeader _ = [(Content System (Message "Calcurate user input, then output just the number. Then call 'output_number' function.") "" defaultUTCTime)]
  customFooter _ = []

-- | Gather the 'customHeader' contents of every custom instruction.
headers :: [CustomInstructionProxy] -> Contents
headers [] = []
headers (tool : tools') =
  case tool of
    (CustomInstructionProxy a) -> customHeader a <> headers tools'

-- | Gather the 'customFooter' contents of every custom instruction.
footers :: [CustomInstructionProxy] -> Contents
footers [] = []
footers (tool : tools') =
  case tool of
    (CustomInstructionProxy a) -> customFooter a <> footers tools'

-- | Gather the 'toolHeader' contents of every tool.
toolHeaders :: [ToolProxy] -> Contents
toolHeaders [] = []
toolHeaders (tool : tools') =
  case tool of
    (ToolProxy (_ :: Proxy a)) -> toolHeader @a <> toolHeaders tools'

-- | Gather the 'toolFooter' contents of every tool.
toolFooters :: [ToolProxy] -> Contents
toolFooters [] = []
toolFooters (tool : tools') =
  case tool of
    (ToolProxy (_ :: Proxy a)) -> toolFooter @a <> toolFooters tools'
