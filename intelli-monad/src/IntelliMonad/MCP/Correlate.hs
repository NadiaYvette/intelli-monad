{-# LANGUAGE OverloadedStrings #-}

-- | Request/response correlation for a JSON-RPC 2.0 connection.
--
-- A JSON-RPC connection multiplexes: requests go out with ids, responses
-- come back arbitrarily later and possibly out of order. This module owns
-- that mapping. It is pure: state in, (actions + state') out.
--
-- Two kinds of inbound traffic are distinguished, because a client must
-- treat them differently:
--
--   * responses to *our* requests ('InboundResponse'), matched by id; and
--   * requests from *the peer* ('InboundRequest') — for MCP these are
--     @sampling\/createMessage@, @roots\/list@, @elicitation\/create@,
--     @ping@ — which the client must answer, not match.
--
-- The implementation is deliberately simple: an 'IntMap' of slots plus a
-- counter, all in plain Haskell values. The caller (the session loop,
-- Phase 1b) owns concurrency and decides what an unfilled expectation
-- means — time it out, retry it, or fail the session.

module IntelliMonad.MCP.Correlate
  ( -- * Correlation state
    Correlator
  , newCorrelator
    -- * Outgoing requests
  , RequestHandle
  , handleId
  , register
    -- * Incoming messages
  , Inbound(..)
  , classify
    -- * Matching
  , MatchResult(..)
  , takeMatching
  , responseKey
    -- * Introspection
  , pendingIds
  , numPending
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Aeson (Value)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Correlator state
--------------------------------------------------------------------------------

-- | Open requests awaiting their response. Keys are the JSON-RPC ids
-- (JSON-RPC 2.0 allows number or string ids; MCP clients conventionally
-- use monotonically increasing numbers, so the int key covers the
-- convention and 'classify' rejects other shapes).
newtype Correlator = Correlator (IntMap Text)

instance Show Correlator where
  show c = "Correlator " <> show (IM.keys (slots c))

instance Eq Correlator where
  a == b = slots a == slots b

slots :: Correlator -> IntMap Text
slots (Correlator m) = m

-- | An empty correlator. Request ids start at 1 (id 0 is legal JSON-RPC
-- but invites off-by-one confusion).
newCorrelator :: Correlator
newCorrelator = Correlator IM.empty

--------------------------------------------------------------------------------
-- Outgoing
--------------------------------------------------------------------------------

-- | An opaque handle to a registered request. Compare with 'handleId'
-- or use 'takeMatching'.
newtype RequestHandle = RequestHandle Int
  deriving (Eq, Ord, Show)

-- | The integer id behind a 'RequestHandle' — for tests and logs;
-- the correlator API itself is opaque.
handleId :: RequestHandle -> Int
handleId (RequestHandle i) = i

-- | Register a request: allocate the next id and record the method so a
-- stray response can be diagnosed.
--
-- >>> import qualified Data.Aeson as A
-- >>> let (_, h, corr) = register newCorrelator "tools/list"
-- >>> handleId h
-- 1
-- >>> numPending corr
-- 1
register :: Correlator -> Text -> (Int, RequestHandle, Correlator)
register (Correlator m) method =
  let nextId = maybe 1 ((1 +) . fst) (IM.lookupMax m)
      h = RequestHandle nextId
  in (nextId, h, Correlator (IM.insert nextId method m))

--------------------------------------------------------------------------------
-- Incoming
--------------------------------------------------------------------------------

-- | A classified inbound message.
data Inbound
  = InboundResponse A.Value A.Object
      -- ^ A response to one of our requests. The 'A.Value' is the raw
      -- JSON-RPC id, exactly as the peer wrote it (number or string —
      -- both legal; our own ids are numbers, server-initiated ids may
      -- not be). The object is the full message for result/error
      -- extraction.
  | InboundRequest Text A.Object
      -- ^ A peer-initiated request (method + full message). The caller
      -- must eventually reply.
  | InboundNotification Text A.Object
      -- ^ A peer notification (no reply expected).
  | Malformed Text
      -- ^ JSON that is none of the above; carries the reason. Never
      -- crashes the stream.
  deriving (Eq, Show)

-- | Classify one decoded JSON value from the wire.
--
-- A success response for one of our requests:
--
-- >>> import qualified Data.Aeson as A
-- >>> let v = A.object ["jsonrpc" A..= "2.0", "id" A..= (1 :: Int), "result" A..= A.Null]
-- >>> let InboundResponse rid _ = classify v
-- >>> responseKey rid
-- Just 1
--
-- A peer-initiated request (string ids are legal for server-initiated
-- requests) and a notification:
--
-- >>> let InboundRequest m _ = classify (A.object ["jsonrpc" A..= "2.0", "id" A..= "srv-1", "method" A..= "ping"])
-- >>> m
-- "ping"
-- >>> let InboundNotification n _ = classify (A.object ["method" A..= "tools/list_changed"])
-- >>> n
-- "tools/list_changed"
--
-- Protocol violations and non-objects are rejected, never crash:
--
-- >>> classify (A.object ["method" A..= "x", "result" A..= A.Null])
-- Malformed "method and result/error co-occur"
-- >>> classify (A.Number 1)
-- Malformed "not an object"
classify :: Value -> Inbound
classify v = case v of
  A.Object o
    -- Protocol violation first: JSON-RPC forbids method on a response and
    -- result/error on a request. Catch the co-occurrence before the
    -- method/id branches can misread the message as a valid shape.
    | KM.member "method" o
    , hasResultOrError o ->
        Malformed "method and result/error co-occur"
    -- A response: has an id, and result or error, and no method.
    | Just rid <- KM.lookup "id" o
    , Nothing <- KM.lookup "method" o
    , hasResultOrError o ->
        InboundResponse rid o
    -- A peer request: method + id (any id shape — server-initiated
    -- requests legally use string ids).
    | Just meth <- methodOf o
    , Just _ <- KM.lookup "id" o ->
        InboundRequest meth o
    -- A notification: method, no id.
    | Just meth <- methodOf o
    , Nothing <- KM.lookup "id" o ->
        InboundNotification meth o
    -- Object-shaped but unclassifiable.
    | otherwise ->
        Malformed (T.pack (show (KM.keys o)))
  _ -> Malformed "not an object"
  where
    methodOf o = case KM.lookup "method" o of
      Just (A.String s) -> Just s
      _ -> Nothing

    hasResultOrError o =
      -- Key presence, not value presence: JSON-RPC makes `result` required
      -- on success even when the result is null, and forbids result+error
      -- co-occurrence, so key membership is the correct discriminator.
      KM.member "result" o || KM.member "error" o

--------------------------------------------------------------------------------
-- Matching
--------------------------------------------------------------------------------

-- | Outcome of 'takeMatching'.
data MatchResult
  = Matched A.Object Correlator
      -- ^ The response object and the correlator with the slot removed.
  | NotMine Correlator
      -- ^ A response id with no open request: stale, duplicate, or from a
      -- previous session. Caller logs and moves on.
  | NotAResponse Correlator
      -- ^ 'takeMatching' was handed something 'classify' calls a response
      -- but whose id has no slot (kept for symmetry in tests).
  deriving (Eq, Show)

-- | Try to match a response-shaped message against open slots.
--
-- A response whose id has an open slot completes it:
--
-- >>> import qualified Data.Aeson as A
-- >>> let (_, _, corr) = register newCorrelator "tools/list"
-- >>> let v = A.object ["jsonrpc" A..= "2.0", "id" A..= (1 :: Int), "result" A..= A.Null]
-- >>> case takeMatching corr (classify v) of Matched _ corr' -> numPending corr'; _ -> -1
-- 0
--
-- An unknown id (stale or duplicate) leaves the correlator untouched:
--
-- >>> let stale = A.object ["jsonrpc" A..= "2.0", "id" A..= (99 :: Int), "result" A..= A.Null]
-- >>> takeMatching corr (classify stale)
-- NotMine Correlator [1]
takeMatching :: Correlator -> Inbound -> MatchResult
takeMatching corr (InboundResponse rid o) =
  case responseKey rid of
    Just mid | IM.member mid (slots corr) ->
      Matched o (Correlator (IM.delete mid (slots corr)))
    _ -> NotMine corr
takeMatching corr _ = NotAResponse corr

-- | Project a wire id onto the correlator's integer key space. Number
-- ids decode to the key; string ids (legal for server-initiated
-- requests) can never collide with our allocated numbers and map to
-- Nothing, reporting NotMine instead of crashing.
responseKey :: A.Value -> Maybe Int
responseKey (A.Number n) = case A.fromJSON (A.Number n) of
  A.Success i -> Just (i :: Int)
  A.Error _ -> Nothing
responseKey _ = Nothing

--------------------------------------------------------------------------------
-- Introspection
--------------------------------------------------------------------------------

-- | Ids with open slots, ascending — for shutdown diagnostics.
pendingIds :: Correlator -> [Int]
pendingIds = IM.keys . slots

-- | Number of requests awaiting a response.
numPending :: Correlator -> Int
numPending = IM.size . slots
