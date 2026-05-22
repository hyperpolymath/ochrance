<!-- SPDX-License-Identifier: MPL-2.0 -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk> -->

# Architecture: Ochrance and the Reversibility Stack

This document addresses tangles P0-P2 from the repo tangle assessment, clarifying
canonical ownership, the reversibility stack layering, and the reposystem boundary.

---

## P0: Canonical Repository Status

**ochrance** is the canonical repository for verified subsystem integrity work.

The former `ochrance-framework` repository has been archived with a redirect notice.
All valuable content (Progressive.idr, Containerfile, flake.nix) was merged into this
repo before archival. The framework repo originally envisioned a broader four-subsystem
architecture (Filesystem, Memory, Network, Crypto), but the concrete filesystem
verification implementation here in ochrance is the active, thesis-scope work.

Going forward, any new subsystem modules (Memory, Network, Crypto) will be developed
as peer repos implementing the `VerifiedSubsystem` interface defined in
`ochrance-core/Ochrance/Framework/Interface.idr`, not in a separate framework repo.

---

## P1: The Reversibility Stack

Three repositories form a layered reversibility stack. Each layer is self-contained
but designed to compose upward:

```
+------------------------------------------------------------+
|  ochrance                                                   |
|  Filesystem verification with dependent types (Idris2)      |
|  Merkle trees, A2ML manifests, progressive assurance        |
|  "Is the filesystem still what we said it was?"             |
+------------------------------------------------------------+
        |  consumes reversibility primitives from
        v
+------------------------------------------------------------+
|  valence-shell (vsh)                                        |
|  Formally verified shell with proven reversibility          |
|  250+ theorems across 6 proof systems, MAA framework        |
|  "Every shell operation can be undone, always"              |
+------------------------------------------------------------+
        |  built on theory from
        v
+------------------------------------------------------------+
|  januskey                                                   |
|  Reversible file operations via Maximal Principle Reduction |
|  Content-addressed storage, transaction support (Rust)      |
|  "Data loss is architecturally impossible"                  |
+------------------------------------------------------------+
```

### Layer Boundaries

**januskey** (bottom layer -- theory and primitives):
- Provides the foundational reversibility guarantee: every file operation carries
  sufficient metadata for perfect inversion.
- Implements content-addressed storage with SHA256 deduplication.
- Transaction manager groups operations for atomic commit/rollback.
- Language: Rust. Interface: CLI (`jk` commands).
- Key property: *data loss is architecturally impossible* -- not unlikely, but
  structurally prevented by storing inverse metadata with every operation.

**valence-shell** (middle layer -- shell/CLI reversibility):
- Extends reversibility from file operations to full shell semantics.
- Proves reversibility theorems in 6 independent proof systems (Coq, Lean 4, Agda,
  Isabelle/HOL, Mizar, Z3) for cross-validation.
- MAA (Mutually Assured Accountability) framework provides provable audit trails.
- Key theorems: `rmdir(mkdir(p, fs)) = fs`, operation sequence composition reversal.
- Language: Rust CLI + formal proofs. Status: advanced research prototype (v0.9.0).
- Trust boundary: formal proofs operate on abstract models; the Lean-to-Rust
  extraction gap is not yet formally verified.

**ochrance** (top layer -- filesystem verification):
- Uses dependent types (Idris2) for mathematically proven filesystem integrity.
- Progressive strictness: Lax (structure only) / Checked (hash verification) /
  Attested (full dependent type proofs).
- A2ML (Attestation & Audit Markup Language) manifests describe verified state.
- Integrates with ECHIDNA for neural proof synthesis.
- Architecture: L0 Hardware -> L1 C Shims -> L2 Idris2 Proofs -> L3 Repair ->
  L4 Policy -> L5 TUI.

### How They Relate

- **januskey** answers: "Can I undo this file operation?" (yes, always)
- **valence-shell** answers: "Can I undo this shell session?" (yes, with proofs)
- **ochrance** answers: "Has anything changed that shouldn't have?" (verified)

januskey provides the *operational* reversibility (undo what you did). valence-shell
extends this to *shell-level* reversibility with formal guarantees. ochrance provides
*verification* that the filesystem matches its declared state, using a different
technique (dependent type proofs over Merkle trees rather than operation-level
inversion).

The stack is loosely coupled: each layer is independently useful, but together they
provide a complete story from "operations are reversible" through "the shell proves it"
to "the filesystem is verified."

---

## P2: Reposystem Boundary

### Inside reposystem (monorepo subdirectories)

The [reposystem](https://github.com/hyperpolymath/reposystem) monorepo contains tools
that form the "railway yard" for multi-repo ecosystem management:

| Subdirectory   | Purpose                                                    |
|----------------|------------------------------------------------------------|
| **contractiles** | Contract-based CLI system (must/trust/dust/intend/k9)    |
| **bitfuckit**    | Bitbucket API integration and forge tooling              |
| **claim-forge**  | Repository claiming and ownership verification           |
| **scaffoldia**   | Project scaffolding and template validation               |

These belong inside reposystem because they are all *repo infrastructure tools* --
they operate on repositories as first-class objects, they share the graph/slot/provider
data model, and they feed into the reposystem TUI and scenario comparison views.

### Standalone (outside reposystem)

**checky-monkey** and **grim-repo** overlap with reposystem's concerns but remain
standalone for the following reasons:

**checky-monkey** (userscript validation platform):
- Operates on a fundamentally different domain: userscripts, userstyles, and browser
  extensions -- not git repositories.
- Has its own persistence layer (CUBS content-addressed database), its own API
  (GraphQL + REST), and its own distribution mechanism (IPFS).
- The overlap with reposystem is limited to sharing the validation/quality-gate
  pattern. checky-monkey validates *scripts*; reposystem validates *repo structure*.
- Technology stack diverges: Haskell core, Lua plugins, PostgreSQL -- versus
  reposystem's Rust CLI + ReScript data model.
- Standalone because: **different domain, different data model, different tech stack.**

**grim-repo** (audit-grade repo tooling):
- Provides modular auditing (structure bootstrapping, community standards, golden
  registry checks) that could theoretically live inside reposystem.
- Remains standalone because it is designed as a *userscript/browser extension*
  that runs on forge web UIs (GitLab, GitHub, Bitbucket pages), not as a CLI or
  library consumed by reposystem.
- Its deployment model (browser injection via Tampermonkey/Violentmonkey) is
  incompatible with being a reposystem subdirectory.
- It follows a progression model (Raw -> Golden -> Rhodium) that is complementary
  to but independent of reposystem's scenario/aspect system.
- Standalone because: **different deployment model (browser extension vs CLI),
  different user interaction pattern (forge web UI vs terminal).**

### Decision Criteria

A tool belongs inside reposystem if it:
1. Operates on git repositories as its primary domain object.
2. Shares the slot/provider/scenario data model.
3. Is consumed as a library or CLI component (not a browser extension).
4. Benefits from the shared graph view and scenario comparison.

A tool remains standalone if it:
1. Has a fundamentally different domain (scripts, not repos).
2. Has an incompatible deployment model (browser vs terminal).
3. Has its own persistence and API requirements.
4. Would create unnecessary coupling if embedded.
