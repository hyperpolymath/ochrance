#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Offline installer boundary tests. Real wheel installation is tested separately.
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

mkdir -p "$work/mock-bin"
cat > "$work/mock-bin/uname" <<'SH'
#!/bin/sh
printf 'Unsupported\n'
SH
chmod +x "$work/mock-bin/uname"
expect_failure 'only Linux' env PATH="$work/mock-bin:$PATH" bash "$INSTALL"
rm "$work/mock-bin/uname"

# Restart path must accept whitespace and not invoke Python or touch the venv.
mkdir -p "$work/installed toolchain/bin"
cat > "$work/installed toolchain/bin/zig" <<'SH'
#!/bin/sh
printf '0.15.2\n'
SH
cat > "$work/mock-bin/python3" <<'SH'
#!/bin/sh
printf 'UNEXPECTED PYTHON INVOCATION\n' >&2
exit 99
SH
chmod +x "$work/installed toolchain/bin/zig" "$work/mock-bin/python3"
env PATH="$work/mock-bin:$PATH" OCHRANCE_ZIG_HOME="$work/installed toolchain" \
  bash "$INSTALL" > "$work/output"
grep -q 'already installed' "$work/output"
[[ ! -e "$work/installed toolchain/venv" ]]

mkdir -p "$work/locked/.install-lock"
expect_failure 'installation locked' env OCHRANCE_ZIG_HOME="$work/locked" bash "$INSTALL"
# A failed bootstrap must release its own lock and preserve existing files.
mkdir -p "$work/failed"
printf 'preserve me\n' > "$work/failed/sentinel"
expect_failure 'UNEXPECTED PYTHON' env PATH="$work/mock-bin:$PATH" \
  OCHRANCE_ZIG_HOME="$work/failed" bash "$INSTALL"
[[ ! -e "$work/failed/.install-lock" ]]
grep -q 'preserve me' "$work/failed/sentinel"
echo 'PASS: 8 offline Zig installer scenarios'
