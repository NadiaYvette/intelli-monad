{-# LANGUAGE OverloadedStrings #-}

-- | Protocol revision negotiation — the compatibility matrix as data.
--
-- MCP has five wire revisions. A client proposes one in @initialize@, the
-- server replies with its own, and every feature decision afterwards is a
-- lookup against the negotiated row. This module is that table.
--
-- The design rule (from docs/negotiation-matrix.md, saved alongside the
-- phase plan): capability gating is a lookup, never an inline version
-- comparison scattered through the code. A new revision means a new row
-- here plus handshake fixtures — not edits across the client.

module IntelliMonad.MCP.Negotiate
  ( -- * Revisions
    Revision(..)
  , revisionString
  , parseRevision
  , allRevisions
    -- * Features
  , Feature(..)
  , supportsFeature
    -- * Negotiation
  , clientDefault
  , Negotiation(..)
  , negotiateWith
  ) where

import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T

-- | The five specification revisions, in chronological order.
-- The 'Ord' instance is chronological, so range checks read directly.
data Revision
  = R2024_11_05
  | R2025_03_26
  | R2025_06_18 -- ^ The issue's target; the client's default proposal.
  | R2025_11_25
  | R2026_07_28 -- ^ Stateless revision.
  deriving (Eq, Ord, Show, Enum, Bounded)

revisionString :: Revision -> Text
revisionString r = case r of
  R2024_11_05 -> "2024-11-05"
  R2025_03_26 -> "2025-03-26"
  R2025_06_18 -> "2025-06-18"
  R2025_11_25 -> "2025-11-25"
  R2026_07_28 -> "2026-07-28"

parseRevision :: Text -> Maybe Revision
parseRevision t = find ((== T.strip t) . revisionString) allRevisions

allRevisions :: [Revision]
allRevisions = [minBound .. maxBound]

-- | Features whose availability the client gates per revision.
data Feature
  = FBatching -- ^ JSON-RPC batch arrays: introduced 2025-03-26, removed 2025-06-18.
  | FStreamableHttp -- ^ Streamable HTTP transport (replaces HTTP+SSE).
  | FHttpSse -- ^ Legacy HTTP+SSE transport (first revision only).
  | FStructuredToolOutput -- ^ @structuredContent@ on tool results.
  | FElicitation -- ^ @elicitation\/create@ exists (reworked in 2025-11-25).
  | FElicitationReworked -- ^ Forms, multi-select, URL mode, defaults.
  | FSamplingTools -- ^ @tools@\/@toolChoice@ on sampling requests.
  | FTasks -- ^ Durable-request tasks (experimental).
  | FStateless -- ^ No session: per-request @_meta@ + @Mcp-Method@ headers.
  | FRequiresInitialize -- ^ The initialize handshake applies.
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The matrix. One clause per feature; each line cites its revision(s).
supportsFeature :: Revision -> Feature -> Bool
supportsFeature r f = case f of
  FBatching -> r == R2025_03_26
  FStreamableHttp -> r >= R2025_03_26
  FHttpSse -> r == R2024_11_05
  FStructuredToolOutput -> r >= R2025_06_18
  FElicitation -> r >= R2025_06_18
  FElicitationReworked -> r >= R2025_11_25
  FSamplingTools -> r >= R2025_11_25
  FTasks -> r >= R2025_11_25 -- experimental
  FStateless -> r == R2026_07_28
  FRequiresInitialize -> r < R2026_07_28

-- | What the client proposes by default: the issue's target revision.
clientDefault :: Revision
clientDefault = R2025_06_18

-- | The outcome of an @initialize@ exchange.
data Negotiation = Negotiation
  { nRequested :: Revision -- ^ What we proposed.
  , nAgreed :: Revision -- ^ What we will actually speak.
  , nNote :: Text -- ^ Human-readable outcome: exact, downgrade, or fallback.
  } deriving (Eq, Show)

-- | Combine our proposal with the server's replied version string.
--
--   * Server's version is one we know and ≤ ours: speak it (exact match
--     or downgrade).
--   * Server's version is unknown or newer than ours: fall back to our
--     proposal and record why. Fixtures (Phase 3) pin the real-world
--     behavior per server; a stricter disconnect policy can replace the
--     fallback without touching any call site.
negotiateWith :: Revision -> Text -> Negotiation
negotiateWith requested serverStr =
  case parseRevision serverStr of
    Just server
      | server <= requested ->
          Negotiation requested server (noteFor server)
      | otherwise ->
          fallback ("server proposed newer " <> serverStr <> "; staying at ours")
    Nothing ->
      fallback ("unknown server version " <> serverStr <> "; staying at ours")
  where
    fallback why =
      Negotiation requested requested (why <> "; using " <> revisionString requested)
    noteFor agreed
      | agreed == requested = "agreed " <> revisionString agreed
      | otherwise =
          "downgraded from " <> revisionString requested
            <> " to " <> revisionString agreed
