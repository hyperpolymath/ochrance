-- SPDX-License-Identifier: MPL-2.0
||| OCHRANCE — ABI Type Definitions
|||
||| This module defines the Application Binary Interface for the Ochrance 
||| security monitor. It ensures that system policy enforcement and 
||| access control checks are handled with formal type-level guarantees.

module Ochrance.ABI.Types

import Data.Bits
import Data.So
import Data.Vect

%default total

--------------------------------------------------------------------------------
-- Platform Context
--------------------------------------------------------------------------------

||| Verified targets for the Ochrance security kernel.
public export
data Platform = Linux | Windows | MacOS | BSD | WASM

||| Resolves the execution environment at compile time.
public export
thisPlatform : Platform
thisPlatform =
  %runElab do
    pure Linux

--------------------------------------------------------------------------------
-- Security Result Codes
--------------------------------------------------------------------------------

||| Formal outcome of a security policy check.
public export
data Result : Type where
  ||| Access Granted
  Ok : Result
  ||| Access Denied: Policy violation
  Error : Result
  ||| Audit Failure: Malformed audit record
  InvalidParam : Result
  ||| Resource Exhaustion
  OutOfMemory : Result
  ||| Safety Error: Unexpected null in security context
  NullPointer : Result

--------------------------------------------------------------------------------
-- Opaque Policy Handles
--------------------------------------------------------------------------------

||| Opaque handle to a Security Policy instance.
||| INVARIANT: The internal pointer is guaranteed to be non-null.
public export
data Handle : Type where
  MkHandle : (ptr : Bits64) -> {auto 0 nonNull : So (ptr /= 0)} -> Handle

||| Safe constructor for security handles.
public export
createHandle : Bits64 -> Maybe Handle
createHandle 0 = Nothing
createHandle ptr = Just (MkHandle ptr)
