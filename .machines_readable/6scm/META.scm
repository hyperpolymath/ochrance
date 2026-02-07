;; SPDX-License-Identifier: PMPL-1.0-or-later
;; META.scm - Meta-level project information

(define meta
  '((architecture-decisions
     ((adr-001
       (status "accepted")
       (date "2026-02-06")
       (context "Language choice for core verification logic")
       (decision "Use Idris2 with %default total for all verification code")
       (consequences "All functions must prove termination. Guarantees
        no infinite loops in verification path. Requires structural
        recursion or sized types for all recursive functions."))

      (adr-002
       (status "accepted")
       (date "2026-02-06")
       (context "Cryptographic hashing for Merkle trees")
       (decision "Support BLAKE3, SHA-256, and SHA3-256 via FFI to C/Zig")
       (consequences "Merkle tree hashing requires FFI. Hash algorithm
        is parameterized so different deployments can choose."))

      (adr-003
       (status "accepted")
       (date "2026-02-06")
       (context "Integration with ECHIDNA theorem prover")
       (decision "Use C FFI to libechidna.so for neural proof synthesis")
       (consequences "Requires libechidna.so to be built and in LD_LIBRARY_PATH.
        Ochránce works standalone for verification; Echidna needed only
        for automated proof generation."))

      (adr-004
       (status "accepted")
       (date "2026-02-06")
       (context "Filesystem repair semantics")
       (decision "Use Idris2 linear types (Quantity 1) for repair operations")
       (consequences "Repair consumes the old filesystem state and produces
        a new one. Prevents use-after-repair bugs at compile time."))

      (adr-005
       (status "accepted")
       (date "2026-02-06")
       (context "Error classification system")
       (decision "Use q/p/z error taxonomy (query/proof/zone)")
       (consequences "Errors are classified by origin: q for user queries
        that fail validation, p for proof failures, z for zone/system
        errors. Enables targeted error handling."))))

    (development-practices
     (code-style "Idris2: %default total, explicit exports, doc comments (|||)")
     (security "SPDX headers, OpenSSF Scorecard, totality checking")
     (testing "Property-based tests, totality proofs as tests, integration tests")
     (versioning "Semantic versioning")
     (documentation "README.adoc, AsciiDoc for specs, inline ||| comments")
     (branching "main branch, feature branches, PRs required"))

    (design-rationale
     ((rationale-001
       (topic "Why dependent types for filesystem verification")
       (explanation "Dependent types allow encoding invariants like
        'Merkle tree has exactly 2^n leaves' directly in the type system.
        The Idris2 compiler then verifies these invariants at compile time,
        catching entire classes of bugs before runtime."))

      (rationale-002
       (topic "Why A2ML as a markup language")
       (explanation "A2ML (Attestation & Audit Markup Language) provides
        a human-readable, machine-parseable format for filesystem manifests.
        Unlike binary formats, A2ML can be inspected, diffed, and version
        controlled. Unlike JSON/YAML, it has a formal grammar with
        type-checked parsing."))))))
