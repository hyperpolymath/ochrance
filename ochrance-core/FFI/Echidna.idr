||| SPDX-License-Identifier: PMPL-1.0-or-later
|||
||| Ochrance.FFI.Echidna - FFI bindings to the ECHIDNA theorem prover
|||
||| Provides bidirectional communication with ECHIDNA's Rust core
||| via libechidna.so. Used for neural proof synthesis when automated
||| proof generation is needed.

module Ochrance.FFI.Echidna

%default total

--------------------------------------------------------------------------------
-- Foreign Declarations
--------------------------------------------------------------------------------

||| Prove a theorem using ECHIDNA's neural synthesis pipeline
%foreign "C:echidna_prove,libechidna"
prim__echidnaProve : String -> PrimIO String

||| Verify a proof witness using ECHIDNA's multi-prover backend
%foreign "C:echidna_verify,libechidna"
prim__echidnaVerify : String -> PrimIO Int

--------------------------------------------------------------------------------
-- Safe Wrappers
--------------------------------------------------------------------------------

||| Attempt to prove a theorem string via ECHIDNA.
||| Returns Left on failure, Right with the proof witness on success.
export
echidnaProve : HasIO io => String -> io (Either String String)
echidnaProve theorem = do
  result <- primIO (prim__echidnaProve theorem)
  if result == ""
    then pure (Left "Proof synthesis failed")
    else pure (Right result)

||| Verify a proof witness via ECHIDNA's prover backends.
||| Returns True if the proof is accepted by at least one prover.
export
echidnaVerify : HasIO io => String -> io Bool
echidnaVerify proof = do
  result <- primIO (prim__echidnaVerify proof)
  pure (result == 1)
