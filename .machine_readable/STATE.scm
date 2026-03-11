;; SPDX-License-Identifier: PMPL-1.0-or-later
;; Ochrance - Neurosymbolic Filesystem Verification Framework
;; STATE.scm - Current project state

(state
  (metadata
    (version "0.2.0")
    (last-updated "2026-03-10")
    (status active))

  (project-context
    (name "ochrance")
    (purpose "Neurosymbolic filesystem verification framework using Idris2 dependent types")
    (completion-percentage 77))

  (components
    (component
      (name "a2ml")
      (description "Attestation & Audit Markup Language")
      (completion-percentage 95)
      (subcomponents
        (subcomponent (name "Lexer") (status "complete") (percentage 100))
        (subcomponent (name "Parser") (status "complete") (percentage 100))
        (subcomponent (name "Validator") (status "complete") (percentage 95))
        (subcomponent (name "Serializer") (status "complete") (percentage 85))))

    (component
      (name "framework")
      (description "Verification framework core")
      (completion-percentage 100)
      (subcomponents
        (subcomponent (name "Interface") (status "complete") (percentage 100))
        (subcomponent (name "Proof") (status "complete") (percentage 100))
        (subcomponent (name "Error") (status "complete") (percentage 100))))

    (component
      (name "filesystem")
      (description "Reference VerifiedSubsystem implementation")
      (completion-percentage 75)
      (subcomponents
        (subcomponent (name "Types") (status "complete") (percentage 100))
        (subcomponent (name "Merkle") (status "active") (percentage 80))
        (subcomponent (name "Verify") (status "active") (percentage 75))
        (subcomponent (name "Repair") (status "active") (percentage 65))))

    (component
      (name "ffi-crypto")
      (description "Zig FFI cryptographic bindings via libochrance.so")
      (completion-percentage 30)
      (notes "FFI declarations present, buffer management needs real implementation"))

    (component
      (name "abi")
      (description "Idris2 ABI definitions with formal proofs")
      (completion-percentage 80)
      (subcomponents
        (subcomponent (name "Types") (status "complete") (percentage 95))
        (subcomponent (name "Layout") (status "complete") (percentage 85))
        (subcomponent (name "Foreign") (status "active") (percentage 60))))

    (component
      (name "echidna-ffi")
      (description "FFI bindings to libechidna.so for neural proof synthesis")
      (completion-percentage 5)
      (notes "Stub only, awaiting libechidna.so stabilization")))

  (blockers-and-issues
    (blocker "FFI buffer management in Crypto.idr uses placeholder cast operations")
    (blocker "Merkle powerTwoSucc arithmetic lemma is postulated, not proven")
    (issue "Verify.idr toHex uses assert_total - needs provably total rewrite")
    (issue "ECHIDNA FFI is stub-only, blocked on libechidna.so API"))

  (critical-next-actions
    (action "Replace Crypto.idr placeholder stubs with real FFI buffer calls")
    (action "Prove powerTwoSucc lemma")
    (action "Rewrite toHex as provably total")
    (action "Complete ABI Foreign module with full FFI coverage")))
