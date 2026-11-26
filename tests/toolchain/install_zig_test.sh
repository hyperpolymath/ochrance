#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Offline installer boundary tests. Real tarball installation is tested separately.
set -euo pipefail
ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)"
INSTALL="$ROOT/.devcontainer/install-zig.sh"
work="$(mktemp -d)"
trap 'rm -rf -- "$work"' EXIT

expect_failure() {
  local expected="$1"
  shift
  if "$@" > "$work/output" 2>&1; then
    echo "FAIL: unexpectedly succeeded: $*" >&2
    exit 1
  fi
  grep -Fq -- "$expected" "$work/output"
}

bash "$INSTALL" --help | grep -q 'No sudo'
expect_failure 'Usage:' bash "$INSTALL" --unknown
expect_failure 'absolute path' env OCHRANCE_ZIG_HOME=relative bash "$INSTALL"
expect_failure 'unsafe installation path' env OCHRANCE_ZIG_HOME=/ bash "$INSTALL"
expect_failure 'must be an https:// URL' env OCHRANCE_ZIG_HOME="$work/mirror" \
  OCHRANCE_ZIG_MIRROR=http://example.invalid bash "$INSTALL"

mkdir -p "$work/mock-bin"
cat > "$work/mock-bin/uname" <<'SH'
#!/bin/sh
printf 'Unsupported\n'
SH
chmod +x "$work/mock-bin/uname"
expect_failure 'only Linux' env PATH="$work/mock-bin:$PATH" bash "$INSTALL"
rm "$work/mock-bin/uname"

# Restart path must accept whitespace and not touch the network.
mkdir -p "$work/installed toolchain/bin"
cat > "$work/installed toolchain/bin/zig" <<'SH'
#!/bin/sh
printf '0.15.2\n'
SH
cat > "$work/mock-bin/curl" <<'SH'
#!/bin/sh
printf 'UNEXPECTED NETWORK ACCESS\n' >&2
exit 99
SH
chmod +x "$work/installed toolchain/bin/zig" "$work/mock-bin/curl"
env PATH="$work/mock-bin:$PATH" OCHRANCE_ZIG_HOME="$work/installed toolchain" \
  bash "$INSTALL" > "$work/output"
grep -q 'already installed' "$work/output"
[[ ! -e "$work/installed toolchain/dist" ]]

mkdir -p "$work/locked/.install-lock"
expect_failure 'installation locked' env OCHRANCE_ZIG_HOME="$work/locked" bash "$INSTALL"
# A failed bootstrap must release its own lock and preserve existing files.
mkdir -p "$work/failed"
printf 'preserve me\n' > "$work/failed/sentinel"
expect_failure 'UNEXPECTED NETWORK' env PATH="$work/mock-bin:$PATH" \
  OCHRANCE_ZIG_HOME="$work/failed" bash "$INSTALL"
[[ ! -e "$work/failed/.install-lock" ]]
grep -q 'preserve me' "$work/failed/sentinel"

# A download that does not match the pinned SHA-256 is never unpacked.
cat > "$work/mock-bin/curl" <<'SH'
#!/bin/sh
while [ $# -gt 0 ]; do
  if [ "$1" = -o ]; then printf 'not a zig tarball\n' > "$2"; exit 0; fi
  shift
done
exit 98
SH
chmod +x "$work/mock-bin/curl"
mkdir -p "$work/tampered"
expect_failure 'checksum mismatch' env PATH="$work/mock-bin:$PATH" \
  OCHRANCE_ZIG_HOME="$work/tampered" bash "$INSTALL"
[[ ! -e "$work/tampered/.install-lock" && ! -e "$work/tampered/dist" && ! -e "$work/tampered/bin" ]]
echo 'PASS: 10 offline Zig installer scenarios'
