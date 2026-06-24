-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
||| OCHRANCE — ABI Type Definitions
|||
||| This module defines the Application Binary Interface for the Ochrance 
||| security monitor. It ensures that system policy enforcement and 
||| access control checks are handled with formal type-level guarantees.

module Ochrance.ABI.Types

import Data.Bits
import Data.So
import Data.Vect
import System.Info

%default total

--------------------------------------------------------------------------------
-- Platform Context
--------------------------------------------------------------------------------

||| Verified targets for the Ochrance security kernel.
public export
data Platform = Linux | Windows | MacOS | BSD | WASM

||| Resolves the execution environment of the running target by mapping the
||| backend's reported OS string onto a verified `Platform`. Unknown or
||| unix-like systems fall back to `Linux`.
public export
thisPlatform : Platform
thisPlatform = case os of
  "windows" => Windows
  "darwin"  => MacOS
  "freebsd" => BSD
  "openbsd" => BSD
  "netbsd"  => BSD
  _         => Linux

--------------------------------------------------------------------------------
-- Primitive Byte & Digest Types
--------------------------------------------------------------------------------

||| A single octet — the atom of every ABI buffer.
public export
Byte : Type
Byte = Bits8

||| A fixed-width hash digest that carries its length in the type, so the FFI
||| boundary can never hand back the wrong number of bytes.
public export
data HashValue : Nat -> Type where
  MkHashValue : Vect n Byte -> HashValue n

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
|||
||| The non-null invariant is discharged at runtime with `choose`: when
||| `ptr /= 0` holds we obtain a `So (ptr /= 0)` witness and hand it to
||| `MkHandle`; otherwise we return `Nothing`. This supplies the proof the
||| old `createHandle 0` / `createHandle ptr` split could not — matching a
||| `Bits64` literal does not refine the variable clause to be non-zero, so
||| the constructor's `auto` search had nothing to find.
public export
createHandle : Bits64 -> Maybe Handle
createHandle ptr = case choose (ptr /= 0) of
  Left  nonNull => Just (MkHandle ptr {nonNull = nonNull})
  Right _       => Nothing
