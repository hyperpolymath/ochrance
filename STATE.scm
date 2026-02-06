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
     (overall-completion 54)
     (components
       (("a2ml-types" 100 "Core A2ML types complete (Manifest, Hash, Ref, Attestation, Policy)")
        ("a2ml-lexer" 100 "Total lexer with fuel-based structural recursion - COMPLETE")
        ("a2ml-parser" 100 "Covering parser with full A2ML support - COMPLETE")
        ("a2ml-validator" 10 "Semantic validation stub (signature verification TODO)")
        ("a2ml-serializer" 0 "Roundtrip serialization stub")
        ("framework-interface" 100 "VerifiedSubsystem interface - COMPLETE")
        ("framework-proof" 100 "Proof witnesses (Lax/Checked/Attested) - COMPLETE")
        ("framework-error" 100 "q/p/z error taxonomy - COMPLETE")
        ("merkle-tree" 30 "Size-indexed Merkle tree (placeholder XOR hash, needs BLAKE3 FFI)")
        ("filesystem-verify" 0 "Filesystem verification logic stub")
        ("filesystem-repair" 0 "Linear type repair stub")
        ("ffi-echidna" 10 "FFI declarations present, wrappers stubbed (libechidna.so needed)")))
     (working-features
       ("A2ML parsing pipeline (lex + parse) fully functional")
       ("Type-safe Manifest AST with validation wrapper")
       ("Error taxonomy with zone-based classification")))

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
       ("Complete A2ML Validator (signature verification via FFI)"
        "Implement A2ML Serializer (roundtrip: Manifest -> String -> Manifest)"
        "Replace Merkle tree XOR placeholder with BLAKE3 FFI"))
     (this-week
       ("Write comprehensive parser tests (fuzzing, edge cases)"
        "Implement Filesystem.Verify (VerifiedSubsystem instance)"
        "Implement Filesystem.Repair with linear types"))
     (this-month
       ("Complete Phase 1 Ochránce Core (all components 100%)"
        "Build libechidna.so and integrate FFI"
        "Begin Phase 2: Echidna Integration")))

    (session-history
      (("2026-02-06" "opus" "Initial repo creation from rsr-template-repo. Renamed to ochrance. GitHub repo created and starred. Set up project structure for neurosymbolic filesystem verification.")
       ("2026-02-06" "opus" "Implemented complete A2ML Parser with covering totality. Parser supports all A2ML sections: @manifest, @refs, @attestation, @policy. Includes field parsing, optional sections, and comprehensive error reporting. Overall progress: 0% -> 54%.")))))
