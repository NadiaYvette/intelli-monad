{-# LANGUAGE AllowAmbiguousTypes #-}
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

module IntelliMonad.Tools
  ( module IntelliMonad.Tools.Utils,
    module IntelliMonad.Tools.Arxiv,
    module IntelliMonad.Tools.Bash,
    module IntelliMonad.Tools.OrganBank,
    defaultTools,
  )
where

import Data.Proxy
import IntelliMonad.Tools.Arxiv
import IntelliMonad.Tools.Bash
import IntelliMonad.Tools.OrganBank
import IntelliMonad.Tools.Utils
import IntelliMonad.Types

-- | The arXiv search tool (see "IntelliMonad.Tools.Arxiv").
arxiv :: ToolProxy
arxiv = ToolProxy (Proxy :: Proxy Arxiv)

-- | The shell-command tool (see "IntelliMonad.Tools.Bash").
bash :: ToolProxy
bash = ToolProxy (Proxy :: Proxy Bash)

-- | The organ-bank OrganIR index tools (see "IntelliMonad.Tools.OrganBank").
organRepoMap :: ToolProxy
organRepoMap = ToolProxy (Proxy :: Proxy OrganRepoMap)

organFindSymbol :: ToolProxy
organFindSymbol = ToolProxy (Proxy :: Proxy OrganFindSymbol)

organCheckBoundary :: ToolProxy
organCheckBoundary = ToolProxy (Proxy :: Proxy OrganCheckBoundary)

organDiagnostics :: ToolProxy
organDiagnostics = ToolProxy (Proxy :: Proxy OrganDiagnostics)

organIngest :: ToolProxy
organIngest = ToolProxy (Proxy :: Proxy OrganIngest)

-- | The default toolchain the REPL exposes and @mcp-serve@ serves.
defaultTools :: [ToolProxy]
defaultTools =
  [ bash,
    arxiv,
    organRepoMap,
    organFindSymbol,
    organCheckBoundary,
    organDiagnostics,
    organIngest
  ]
