;; SPDX-License-Identifier: PMPL-1.0-or-later
(bot-directive
  (bot "echidnabot")
  (scope "formal verification, proof totality, and dependent type auditing")
  (languages ("Idris2" "Zig"))
  (targets
    ("ochrance-core/" "Idris2 core library — A2ML, Framework, FFI")
    ("modules/" "Verified filesystem subsystem")
    ("src/abi/" "ABI type definitions")
    ("ffi/zig/" "Zig FFI implementation")
    ("tests/" "Test suite"))
  (allow ("analysis" "proof checks" "totality auditing" "FFI safety checks"))
  (deny ("write to core modules" "write to FFI bindings" "modify proofs without review"))
  (scanning-rules
    (idris2
      (ban ("believe_me" "assert_total" "assert_smaller" "unsafePerformIO") (severity "critical"))
      (enforce ("total" "%default total") (notes "All modules must be total"))
      (flag ("partial") (severity "high")))
    (zig
      (flag ("@ptrCast" "@alignCast") (severity "medium"))
      (ban ("@intToPtr") (severity "high") (unless "FFI boundary"))))
  (echidna-ffi
    (library "libechidna.so")
    (bindings "ochrance-core/Ochrance/FFI/Echidna.idr")
    (status "stub — FFI not yet implemented, wrappers return defaults"))
  (notes "Ochrance is a neurosymbolic verification framework. Echidna integration is core to its architecture. All functions must be total — structural recursion only."))
