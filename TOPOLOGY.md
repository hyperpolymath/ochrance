<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- TOPOLOGY.md — Project architecture map and completion dashboard -->
<!-- Last updated: 2026-02-19 -->

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

```
COMPONENT                          STATUS              NOTES
─────────────────────────────────  ──────────────────  ─────────────────────────────────
CORE VERIFICATION (IDRIS2)
  Merkle Tree Implementation        ██████████ 100%    Size-indexed proofs stable
  Linear Type Repair                ████████░░  80%    Use-after-repair prevention
  A2ML Parser                       ██████████ 100%    Attestation DSL verified
  VerifiedSubsystem Interface       ██████████ 100%    Abstract interface stable

SYSTEM & EXTERNAL
  Zig FFI Bridge                    ██████████ 100%    Stable C ABI bridge
  ECHIDNA Integration               ██████░░░░  60%    Proof synthesis refining
  Filesystem Module                 ████████░░  80%    Reference implementation stable

REPO INFRASTRUCTURE
  Justfile Automation               ██████████ 100%    Standard build/verify tasks
  .machine_readable/                ██████████ 100%    STATE tracking active
  Idris2 .ipkg                      ██████████ 100%    Package definitions verified

─────────────────────────────────────────────────────────────────────────────
OVERALL:                            ████████░░  ~80%   Framework stable, ML maturing
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
