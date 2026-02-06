;; SPDX-License-Identifier: PMPL-1.0-or-later
;; ECOSYSTEM.scm - Project ecosystem position

(ecosystem
 (version "1.0")
 (name "ochrance")
 (type "framework")
 (purpose "Dependent-type filesystem verification using Idris2, with
  neurosymbolic proof assistance via ECHIDNA integration")

 (position-in-ecosystem
  "Ochránce is the formal verification layer for filesystem integrity.
   It produces and validates A2ML manifests (Attestation & Audit Markup
   Language) containing Merkle tree roots and cryptographic attestations.
   Ochránce provides the Idris2 side of the Ochránce-Echidna integration,
   while the echidna repo provides the Rust/Julia side.")

 (related-projects
  ((name "echidna")
   (relationship "sibling-integration")
   (description "Neurosymbolic theorem proving platform (Rust/Julia).
    Ochránce calls Echidna via FFI for neural proof synthesis."))
  ((name "idris2-echidna")
   (relationship "sibling-binding")
   (description "Idris2 bindings for the Echidna prover abstraction layer.
    Ochránce uses these for multi-prover verification."))
  ((name "proven")
   (relationship "core-dependency")
   (description "Idris2 formally verified library.
    Ochránce builds on proven's verified primitives."))
  ((name "absolute-zero")
   (relationship "potential-consumer")
   (description "Coq formalization of physics.
    ECHIDNA test case for cross-validation.")))

 (what-this-is
  "A framework for proving filesystem integrity using dependent types.
   Provides: A2ML markup language for attestation manifests, verified
   Merkle trees, filesystem state verification, linear-type repair,
   and integration with the ECHIDNA neural proof synthesizer.")

 (what-this-is-not
  "Not a general theorem prover (that's ECHIDNA). Not a filesystem
   implementation (it verifies existing filesystems). Not a backup
   tool (it detects corruption, not recovers data)."))
