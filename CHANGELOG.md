<!--
SPDX-License-Identifier: CC-BY-SA-4.0
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
-->

# Changelog

All notable changes to `ochrance` will be documented in this file.

This file is generated from conventional commits by the
[`changelog-reusable.yml`](https://github.com/hyperpolymath/standards/blob/main/.github/workflows/changelog-reusable.yml)
workflow (`hyperpolymath/standards#206`). Adopt the workflow in this repo's CI to keep this file in sync automatically — see
[`templates/cliff.toml`](https://github.com/hyperpolymath/standards/blob/main/templates/cliff.toml)
for the canonical config.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
this project aims to follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- feat(crg): add crg-grade and crg-badge justfile recipes
- feat: add stapeln.toml layer-based container definition\n\nConverted from existing Containerfile to stapeln format.\nIncludes Chainguard base, security hardening, SBOM generation.\n\nCo-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>
- feat: deploy UX Manifesto infrastructure
- feat: add first seam record (ochrance-januskey)
- feat: merge Progressive.idr, Containerfile, flake.nix from ochrance-framework
- feat: add CLADE.a2ml — clade taxonomy declaration
- feat: complete Phase 1 - dependent types, linear types, FFI buffers, end-to-end test
- feat: implement hex string parsing for Ed25519 signatures
- feat: add Ed25519 signature verification to FFI
- feat: successful Phase 1 build with Idris2 + Zig FFI

### Fixed

- fix(ci): Phase-2 fleet submission must not fail the security gate (#12)
- fix(ci): hypatia-scan workdir (${{ env.HOME }} resolves empty) (#11)
- fix(ci): hypatia-scan.yml -- pass GITHUB_TOKEN, use --exit-zero (hyperpolymath/hypatia#213) (#6)
- fix(ci): bump erlef/setup-beam SHA for ubuntu24 runner support (#7)
- fix(ci): repair YAML block-scalar in workflow-linter Check Permissions step (#8)
- fix(ci): move secret-scanner Cargo.toml gate from job-level if: to step-level (#9)
- fix(ci): resolve 6 workflow failures across CI pipeline
- fix(scorecard): enforce granular permissions and add fuzzing placeholder
- fix(ci): Resolve workflow-linter self-matching and metadata issues
- fix: correct email jonathan.jewell → j.d.a.jewell

### Changed

- refactor: migrate 6SCM → 6A2 (.scm → .a2ml format)
- refactor: fix SCM file duplication and update documentation

### Documentation

- docs: update TEST-NEEDS.md with session 9 E2E CI additions
- docs: substantive CRG C annotation (EXPLAINME.adoc)
- docs: add TEST-NEEDS.md and/or PROOF-NEEDS.md from audit
- docs: add EXPLAINME.adoc — prove-it file backing README claims
- docs: add ARCHITECTURE.md with reversibility stack + reposystem boundary
- docs: resolve P0 tangle — document complementary relationship with ochrance-framework
- docs: update STATE.scm - Phase 1 complete, Phase 2 started
- docs: add AI Gatekeeper Protocol notes and foundations status

### CI

- ci(secret-scanner): drop duplicate --fail from trufflehog extra_args (#5)
- ci: SHA-pin hyperpolymath validate-actions in dogfood-gate
- ci: restore Dependabot security path + wire auto-merge
- ci(e2e): add just e2e recipe wired to tests/e2e_test.sh
- ci: add E2E/property/aspect CI workflow

## Pre-history

Prior commits to this file's introduction are recorded in git history but not formally classified into Keep-a-Changelog sections. To backfill, run `git cliff -o CHANGELOG.md` locally using the canonical [`cliff.toml`](https://github.com/hyperpolymath/standards/blob/main/templates/cliff.toml) — this is one-shot mechanical work.

---

<!-- This file was seeded by the 2026-05-26 estate tech-debt audit follow-up (Row-2 Phase 3); see [`hyperpolymath/standards/docs/audits/2026-05-26-estate-documentation-debt.md`](https://github.com/hyperpolymath/standards/blob/main/docs/audits/2026-05-26-estate-documentation-debt.md). -->
