{-# LANGUAGE OverloadedStrings #-}

-- | Wire framing for the MCP stdio transport.
--
-- MCP transports carry JSON-RPC 2.0 messages. The stdio transport uses
-- newline-delimited JSON: one message per line, no embedded newlines.
-- This module provides the pure parts of that framing — an encoder, a
-- whole-buffer decoder, and an incremental line assembler for the real
-- read loop, where OS reads never align with message boundaries.
--
-- A malformed line can never corrupt stream state: the assembler
-- resynchronizes at the next newline and reports what it dropped.
-- Empty lines are framing noise and are skipped.
--
-- This module is intentionally pure: no handles, no IO. The transport
-- layer (Phase 1b) feeds it chunks and ships the decoded values onward.

module IntelliMonad.MCP.Framing
  ( -- * Encoding
    encodeMessage
    -- * Whole-buffer decoding
  , DecodeError(..)
  , decodeBuffer
    -- * Incremental assembly
  , LineAssembler
  , newLineAssembler
  , assemblerPending
  , feedChunk
  , AssemblerEvent(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (Value)
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE

-- | Encode one JSON value as a newline-delimited frame.
--
-- The encoded message contains no embedded newlines (aeson's encoder
-- never emits raw newlines inside strings), satisfying the stdio spec's
-- framing requirement.
--
-- >>> import qualified Data.Aeson as A
-- >>> import qualified Data.ByteString as BS
-- >>> import qualified Data.Text as T
-- >>> BS.putStr (encodeMessage (A.object ["jsonrpc" A..= ("2.0" :: T.Text)]))
-- {"jsonrpc":"2.0"}
encodeMessage :: A.ToJSON a => a -> ByteString
encodeMessage = BL.toStrict . (<> "\n") . A.encode

-- | A line that failed to parse, preserved for diagnostics.
data DecodeError = DecodeError
  { dePayload :: Text -- ^ the raw line that failed to parse
  , deMessage :: Text -- ^ why it failed
  } deriving (Eq, Show)

-- | Decode a whole buffer of newline-delimited JSON.
--
-- Returns, in order:
--
--   * every complete line that parsed as JSON, and
--   * every complete line that did not, as a 'DecodeError'.
--
-- The final component is any trailing bytes after the last newline —
-- an incomplete message the caller should expect to complete later.
-- Empty lines are skipped.
--
-- >>> import qualified Data.Aeson as A
-- >>> let (ok, bad, trailing) = decodeBuffer "{\"id\":1}\ngarbage line\n"
-- >>> map A.encode ok
-- ["{\"id\":1}"]
-- >>> map dePayload bad
-- ["garbage line"]
-- >>> trailing
-- Nothing
--
-- Bytes after the final newline come back in the third component:
--
-- >>> let (_, _, partial) = decodeBuffer "{\"id\":1}\npartial"
-- >>> partial
-- Just "partial"
decodeBuffer
  :: ByteString
  -> ([Value], [DecodeError], Maybe ByteString)
decodeBuffer buf = (values, errors, trailing')
  where
    (lines', rawTrailing) = splitLines buf
    trailing' = if BS.null rawTrailing then Nothing else Just rawTrailing
    (values, errors) = foldr step ([], []) lines'
    step l (vs, es)
      | BS.null l = (vs, es)
      | otherwise =
          case A.eitherDecode' (BL.fromStrict l) of
            Right v -> (v : vs, es)
            Left e ->
              ( vs
              , DecodeError { dePayload = lenientDecode l
                            , deMessage = T.pack e
                            } : es
              )

-- | Incremental line assembler for a streaming read loop.
newtype LineAssembler = LineAssembler ByteString
  deriving (Eq, Show)

-- | Bytes held waiting for a newline.
assemblerPending :: LineAssembler -> ByteString
assemblerPending (LineAssembler b) = b

-- | An assembler holding no partial line — the initial state.
newLineAssembler :: LineAssembler
newLineAssembler = LineAssembler BS.empty

-- | One complete line is ready (without the newline). Never empty:
-- empty lines are framing noise, consumed silently.
data AssemblerEvent = LineComplete ByteString
  deriving (Eq, Show)

-- | Feed a raw read chunk into the assembler, extracting every complete
-- line it finishes. Whatever follows the last newline stays buffered.
-- Empty lines are consumed silently.
--
-- >>> let (ev1, a1) = feedChunk newLineAssembler "{\"id\":1}\n{\"id\":"
-- >>> ev1
-- [LineComplete "{\"id\":1}"]
-- >>> assemblerPending a1
-- "{\"id\":"
-- >>> let (ev2, a2) = feedChunk a1 "2}\n\n"
-- >>> ev2
-- [LineComplete "{\"id\":2}"]
-- >>> assemblerPending a2
-- ""
feedChunk
  :: LineAssembler
  -> ByteString
  -> ([AssemblerEvent], LineAssembler)
feedChunk (LineAssembler old) chunk =
  let (lines', trailing) = splitLines (old <> chunk)
      events = [LineComplete l | l <- lines', not (BS.null l)]
  in (events, LineAssembler trailing)

-- | Split at newlines: all complete lines (newline removed), plus the
-- trailing bytes after the final newline (empty if the buffer ends on
-- a newline).
splitLines :: ByteString -> ([ByteString], ByteString)
splitLines = go id
  where
    go acc bs =
      case BS.elemIndex 10 bs of
        Nothing -> (acc [], bs)
        Just i  -> go (acc . (BS.take i bs :)) (BS.drop (i + 1) bs)

-- | Decode bytes as text leniently, replacing invalid sequences, so an
-- error report never fails on binary garbage.
lenientDecode :: ByteString -> Text
lenientDecode = TE.decodeUtf8With TEE.lenientDecode
