#!/bin/bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# SessionStart hook — provision the Ochránce toolchain for Claude Code on the web.
#
# The web container is ephemeral and cloned fresh, so the toolchains this repo
# needs are reinstalled at session start:
#   - Idris2  v0.8.0  (core framework, proofs, parser, verification)
#   - Zig     0.15.2  (cryptographic FFI in ffi/zig)
#   - just            (build/recipe runner; `just build`, `just check`, ...)
#   - Agda    2.6.3   (estate formal-verification toolchain)
#
# Idempotent: each tool is skipped when already present, so cached containers
# start fast. Best-effort: a failure in one tool is logged but does not abort
# the hook (a partial toolchain beats a blocked session).
#
# Network-policy notes for the web environment (do not "fix" these away):
#   - ziglang.org and api.github.com are BLOCKED; github.com and pypi.org are
#     reachable. Zig therefore comes from the PyPI `ziglang` package (not
#     ziglang.org), and Idris2 is built from GitHub source (not a prebuilt).
#   - `apt-get update` fails (the distro index mirror is blocked) but
#     `apt-get install` works off the image's cached package lists, so update
#     is best-effort only.
set -uo pipefail

# Web-only: on a local machine, use your own toolchain.
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

log()  { printf '[session-start] %s\n' "$*" >&2; }
have() { command -v "$1" >/dev/null 2>&1; }

# --- apt packages: just, Agda, and the Idris2 build dependencies ------------
if ! { have just && have agda && have idris2; }; then
  log "installing apt packages (just, agda, Idris2 build deps) ..."
  apt-get update >/dev/null 2>&1 || true
  apt-get install -y --no-install-recommends \
    just agda agda-stdlib chezscheme libgmp-dev make gcc git \
    || log "WARN: some apt packages failed to install"
fi

# --- Zig 0.15.2 via the PyPI ziglang package (ziglang.org is blocked) -------
if ! have zig; then
  log "installing Zig 0.15.2 via PyPI ziglang ..."
  if pip3 install --break-system-packages --quiet "ziglang==0.15.2"; then
    cat > /usr/local/bin/zig <<'SH'
#!/bin/sh
# Thin shim exposing the PyPI `ziglang` package as a `zig` command.
exec python3 -m ziglang "$@"
SH
    chmod +x /usr/local/bin/zig
  else
    log "WARN: Zig install failed"
  fi
fi

# --- Idris2 v0.8.0, built from GitHub source (not in apt; no reachable bin) --
# Cold build self-hosts the compiler and takes several minutes; cached
# containers skip it. Needs chezscheme + libgmp-dev (installed above).
if ! have idris2; then
  log "building Idris2 v0.8.0 from source (several minutes on a cold container) ..."
  src=/opt/Idris2-build
  if rm -rf "$src" \
     && git clone --depth 1 --branch v0.8.0 https://github.com/idris-lang/Idris2 "$src" \
     && make -C "$src" bootstrap SCHEME=chezscheme PREFIX=/usr/local \
     && make -C "$src" install PREFIX=/usr/local; then
    log "Idris2 installed to /usr/local"
  else
    log "WARN: Idris2 build failed (see output above)"
  fi
fi

# --- Report -----------------------------------------------------------------
log "toolchain status:"
have zig    && log "  zig    $(zig version 2>&1)"                || log "  zig    MISSING"
have just   && log "  just   $(just --version 2>&1)"             || log "  just   MISSING"
have agda   && log "  agda   $(agda --version 2>&1 | head -1)"   || log "  agda   MISSING"
have idris2 && log "  idris2 $(idris2 --version 2>&1 | head -1)" || log "  idris2 MISSING"

exit 0
