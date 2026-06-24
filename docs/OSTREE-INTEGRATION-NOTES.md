<!--
SPDX-License-Identifier: CC-BY-SA-4.0
Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->
<!-- Author: Jonathan D.A. Jewell -->
# OSTree Integration Notes (Phase 4 Preparation)

Preparation document for Ochrance Phase 4 (v1.0.0) OSTree integration.
This is a living document — update as the OSTree API evolves.

## Planned API Surface

Based on the ROADMAP.adoc Phase 4 plan, Ochrance will interact with OSTree via:

### Transaction Hooks (`/etc/ostree/hooks/`)

| Hook Point | Ochrance Action |
|---|---|
| **Pre-transaction** | Snapshot current Merkle tree state, validate manifest signatures |
| **Post-transaction** | Rebuild Merkle tree for updated deployment, verify integrity |

### Systemd Integration

- **Continuous verification service**: Periodic Merkle tree re-verification
- **Journal logging**: Structured audit logs for verification results (pass/fail/repair)

### OSTree CLI / Library Calls

Ochrance will invoke or link against:

| API | Purpose | Stability |
|---|---|---|
| `ostree admin deploy` exit hooks | Trigger post-deploy verification | Stable (hook dirs) |
| `ostree ls` / `ostree diff` | Enumerate deployment contents for Merkle input | Stable |
| `ostree commit` metadata | Embed Ochrance attestation in commit metadata | Stable |
| `libostree` GObject API (`OstreeRepo`, `OstreeSysroot`) | Direct library access (if FFI proves necessary) | Stable, versioned |

## Current Stable OSTree API Version

As of 2026, the relevant stable releases:

- **libostree**: 2025.x series (year-based versioning since 2023)
- **API stability**: libostree follows a strict ABI stability policy — symbols are never removed from stable releases
- **GObject Introspection**: Full GIR bindings available (`OSTree-1.0.gir`)

Key version note: libostree moved to year-based versioning (e.g., `2024.1`, `2025.3`).
Pin to a minimum of `2024.1` for any direct library linking.

## Known Deprecations and Upcoming Changes

1. **GPG signing deprecated in favour of Ed25519 (signapi)**
   - `ostree sign` now prefers Ed25519 over GPG
   - Ochrance already uses Ed25519 (via Zig FFI) — this is aligned
   - Migration: Use `ostree sign --sign-type=ed25519` when signing commits

2. **Composefs integration (experimental → stabilising)**
   - OSTree is gaining native composefs (erofs+overlayfs) support
   - This changes how deployments are mounted but NOT the commit/metadata API
   - Impact on Ochrance: Merkle verification inputs may come from composefs manifests
     rather than traditional checkout directories
   - Action: Monitor `ostree_repo_checkout_composefs()` API stability

3. **Boot counting / rollback changes**
   - Automatic rollback on failed boots is becoming standard
   - Ochrance should integrate with boot-counting to trigger verification
     on first boot of a new deployment

4. **No planned removals** of `ostree ls`, `ostree diff`, or hook directories

## Migration Strategy

### If OSTree Hook API Changes

Current plan uses simple hook scripts in `/etc/ostree/hooks/`. If this mechanism
changes:

1. **Fallback to systemd path units**: Watch `/ostree/deploy/` for changes
   using `systemd.path` units — this is OSTree-version-independent
2. **Fallback to inotify**: Direct filesystem monitoring of sysroot
3. **libostree signal handlers**: Connect to GObject signals if using
   direct library integration

### If Composefs Becomes Default

1. Ochrance Merkle tree inputs should abstract over the deployment source
2. Define a `DeploymentSource` interface in Idris2 that can read from:
   - Traditional OSTree checkout directories
   - Composefs manifest files (erofs images)
3. Phase 4 implementation should build both backends

### If Ed25519 Signing API Changes

Ochrance's Ed25519 FFI is self-contained (Zig stdlib, not linked to libostree).
If OSTree changes its Ed25519 wire format:

1. The Zig FFI functions (`ed25519_verify`) operate on raw bytes — format-agnostic
2. Only the integration layer that reads OSTree commit signatures would need updating
3. Keep signature parsing separate from verification logic

## References

- [libostree API reference](https://ostreedev.github.io/ostree/)
- [OSTree composefs design](https://github.com/ostreedev/ostree/blob/main/doc/composefs.md)
- [Ed25519 sign API](https://ostreedev.github.io/ostree/signing/)
- [Fedora Kinoite](https://fedoraproject.org/kinoite/) — primary deployment target
