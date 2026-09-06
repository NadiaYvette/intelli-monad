-- The Haskell island for the gold loop. Real GHC-compiled code.
--
-- `factorialImpl` is the island's own logic. `hs_factorial` is the
-- island's real exported entry — the name organ_plan_stub fills the
-- callee trampoline with when crossing the other way. `rsCall` is the
-- transplanted path: a foreign import of the wire-generated caller
-- glue, which forwards into the Rust island through the filled Rust
-- trampoline.
{-# LANGUAGE ForeignFunctionInterface #-}

module Factorial where

import Foreign.C.Types

factorialImpl :: CLong -> CLong
factorialImpl n = go n 1
  where
    go 0 acc = acc
    go k acc = go (k - 1) (acc * k)

-- The island's real entry. Its name is passed as opsCalleeExport when
-- the crossing is planned rust→haskell.
foreign export ccall hs_factorial :: CLong -> IO CLong
hs_factorial :: CLong -> IO CLong
hs_factorial = return . factorialImpl

-- The transplanted path: Haskell calls the wire-generated caller glue
-- (caller.c), which externs the filled callee trampoline (callee.c),
-- which forwards to the Rust island's real entry (factorial.rs).
foreign import ccall unsafe "omni_haskell_Factorial_factorial"
  glueCall :: CLong -> IO CLong

foreign export ccall rsCall :: CLong -> IO CLong
rsCall :: CLong -> IO CLong
rsCall = glueCall
