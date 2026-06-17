<!-- SPDX-License-Identifier: MPL-2.0 -->
<!-- TOPOLOGY.md — Project architecture map and completion dashboard -->
<!-- Last updated: 2026-06-17 -->

# Ochránce — Project Topology

## System Architecture

```
                        ┌─────────────────────────────────────────┐
                        │              OPERATOR / ADMIN           │
                        │        (Filesystem Audit / Verification)│
                        └───────────────────┬─────────────────────┘
                                            │
                                            ▼
                        ┌─────────────────────────────────────────┐
                        │           OCHRÁNCE CORE (IDRIS2)        │
                        │    (Dependent Types, Formal Proofs)     │
                        └──────────┬───────────────────┬──────────┘
                                   │                   │
                                   ▼                   ▼
                        ┌───────────────────────┐  ┌────────────────────────────────┐
                        │ INTEGRITY LAYER       │  │ MARKUP LAYER (A2ML)            │
                        │ - Verified Merkle     │  │ - Attestation DSL              │
                        │ - Linear Repair Ops   │  │ - Audit Manifests              │
                        │ - Size-indexed Trees  │  │ - Parser / Validator           │
                        └──────────┬────────────┘  └──────────┬─────────────────────┘
                                   │                          │
                                   └────────────┬─────────────┘
                                                ▼
                        ┌─────────────────────────────────────────┐
                        │           INTERFACE LAYER (FFI)         │
                        │  ┌───────────┐  ┌───────────────────┐  │
                        │  │  Zig FFI  │  │  C ABI Bridge     │  │
                        │  │  (System) │  │  (Shared Libs)    │  │
                        │  └─────┬─────┘  └────────┬──────────┘  │
                        └────────│─────────────────│──────────────┘
                                 │                 │
                                 ▼                 ▼
                        ┌─────────────────────────────────────────┐
                        │           ECHIDNA INTEGRATION           │
                        │    (Neural Proof Synthesis, Julia ML)   │
                        └─────────────────────────────────────────┘

                        ┌─────────────────────────────────────────┐
                        │          REPO INFRASTRUCTURE            │
                        │  Justfile Automation  .machine_readable/  │
                        │  Idris2 ipkg          0-AI-MANIFEST.a2ml  │
                        └─────────────────────────────────────────┘
```

## Completion Dashboard

> Percentages track **proof / verification completeness** — the honest axis for a
> verification framework — **not** "code exists". `docs/PROOFS.adoc` is the
> authoritative ledger; this dashboard only summarises it. Where an implementation
> exists but its correctness is unproven (or is a stub), the bar reflects the
> *proof*, not the code.

```
COMPONENT                            STATUS         NOTES (proof / verification axis)
───────────────────────────────────  ──────────────  ────────────────────────────────────────────
CORE VERIFICATION (IDRIS2)
  Merkle soundness + round-trip       ███████░░░ 70%  merkleCorrect(With) + buildGetLeaf proven
  Merkle completeness + binding       ░░░░░░░░░░  0%  converse + CollisionResistant (Stage 1.4/4)
  A2ML parser + reference round-trip  ███████░░░ 70%  total; ref-codec proven; prod pipeline runtime-only
  Verify soundness                    ░░░░░░░░░░  0%  verify success ⇏ a proof yet (Stage 2)
  Linear-type Repair                  ██░░░░░░░░ 20%  IMPL IS A STUB (no block I/O); unproven (Stage 3)
  VerifiedSubsystem law               █████░░░░░ 50%  interface + FSState instance; law unproven (Stage 3)
  Progressive assurance               ███████░░░ 70%  attestedSatisfiesLax proven; monotonicity (Stage 4)

SYSTEM & EXTERNAL
  Zig crypto (BLAKE3/SHA/Ed25519)     ███████░░░ 70%  implemented + known-answer-vector tested in Zig
  Crypto linked into verify flow      ██░░░░░░░░ 20%  NOT linked; Idris-side stubs still live (#39)
  ECHIDNA integration                 █░░░░░░░░░ 10%  design types only; FFI entirely stubbed

REPO INFRASTRUCTURE
  Justfile automation                 ██████████ 100% build / verify tasks
  .machine_readable/                  ██████████ 100% STATE tracking active
  Idris2 .ipkg / totality gate        ██████████ 100% 19 core modules; --total build green

────────────────────────────────────────────────────────────────────────────────────────────────
OVERALL (proof axis):                 ████░░░░░░ ~40% Stage 1.1 done; 1.2–4 open. See docs/PROOFS.adoc.
```

## Key Dependencies

```
A2ML Manifest ───► Idris2 Parser ───► Merkle Root ───► Attestation
     │                 │                 │                │
     ▼                 ▼                 ▼                ▼
Linear Repr ─────► Verified FS ──────► C FFI ───────► ECHIDNA (ML)
```

## Update Protocol

This file is maintained by both humans and AI agents. When updating:

1. **After completing a component**: Change its bar and percentage
2. **After adding a component**: Add a new row in the appropriate section
3. **After architectural changes**: Update the ASCII diagram
4. **Date**: Update the `Last updated` comment at the top of this file

Progress bars use: `█` (filled) and `░` (empty), 10 characters wide.
Percentages: 0%, 10%, 20%, ... 100% (in 10% increments).

The bars track **proof / verification completeness** against `docs/PROOFS.adoc`
(the authoritative ledger), not whether code merely exists. Do not raise a bar to
100% on the strength of an implementation alone — a component is "done" only when
its correctness is proven or honestly bounded per the ledger.
