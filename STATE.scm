;; SPDX-License-Identifier: PMPL-1.0-or-later
;; STATE.scm - Current project state

(define state
  '((metadata
     (version "0.1.0")
     (schema-version "1.0")
     (created "2026-02-06")
     (updated "2026-02-06")
     (project "ochrance")
     (repo "hyperpolymath/ochrance"))

    (project-context
     (name "Ochránce")
     (tagline "Neurosymbolic Filesystem Verification with Dependent Types")
     (tech-stack ("Idris2" "Rust" "Julia" "Zig")))

    (current-position
     (phase "Phase 1: Ochránce Core")
     (overall-completion 0)
     (components
       (("a2ml-lexer" 0 "Total lexer with structural recursion")
        ("a2ml-parser" 0 "Total parser with sized types")
        ("a2ml-validator" 0 "Semantic validation of manifests")
        ("a2ml-serializer" 0 "Roundtrip serialization")
        ("a2ml-types" 0 "Core A2ML types (Manifest, Hash)")
        ("framework-interface" 0 "VerifiedSubsystem interface")
        ("framework-proof" 0 "Proof witnesses")
        ("framework-error" 0 "q/p/z error system")
        ("merkle-tree" 0 "Verified Merkle tree with proofs")
        ("filesystem-verify" 0 "Filesystem verification logic")
        ("filesystem-repair" 0 "Linear type repair")
        ("ffi-echidna" 0 "FFI bindings to Echidna (Rust)")))
     (working-features ()))

    (route-to-mvp
     (milestones
       ((milestone-id "phase-1")
        (name "Ochránce Core")
        (status "in-progress")
        (items ("A2ML Lexer (total, tested)"
                "A2ML Parser (total, tested)"
                "A2ML Validator (comprehensive)"
                "A2ML Serializer (roundtrip verified)"
                "Merkle tree implementation"
                "Filesystem verification"
                "Linear type repair"
                "Integration tests (50+ scenarios)")))
       ((milestone-id "phase-2")
        (name "Echidna Integration")
        (status "planned")
        (items ("FFI (Idris2 -> Rust via libechidna)"
                "Bidirectional proof exchange"
                "Integration tests (100+ proofs)")))
       ((milestone-id "phase-3")
        (name "Neural Synthesis Support")
        (status "planned")
        (items ("Proof corpus (200+ Idris2 examples)"
                "Synthesis pipeline integration"
                "Evaluation benchmarks")))
       ((milestone-id "phase-4")
        (name "Production Deployment")
        (status "planned")
        (items ("OSTree integration"
                "Performance benchmarks"
                "Security audit"
                "Thesis document")))))

    (blockers-and-issues
     (critical ())
     (high
       ("Idris2 compiler (0.8.0+) must be installed"
        "libechidna.so not yet built (blocks Phase 2 FFI)"))
     (medium
       ("Need BLAKE3/SHA-256 FFI for Merkle tree hashing"))
     (low ()))

    (critical-next-actions
     (immediate
       ("Implement A2ML Types module"
        "Implement A2ML Lexer (total)"
        "Set up ochrance.ipkg"))
     (this-week
       ("Implement A2ML Parser"
        "Write lexer/parser tests"
        "Implement Merkle tree core"))
     (this-month
       ("Complete Phase 1 Ochránce Core"
        "Begin Echidna FFI integration")))

    (session-history
      (("2026-02-06" "opus" "Initial repo creation from rsr-template-repo.
        Set up project structure for neurosymbolic filesystem verification.")))))
