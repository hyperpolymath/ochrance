#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Opt-in, user-local Zig installation for restricted Linux web sessions.
set -euo pipefail

usage() {
  cat <<'HELP'
Usage: bash .devcontainer/install-zig.sh
Install Zig 0.15.2 from checksum-pinned PyPI wheels in a private Python venv.
Requires Linux x86_64/aarch64, python3 with venv/pip, and PyPI network access.
No sudo, system Python changes, shell-profile edits, or automatic activation.
Set OCHRANCE_ZIG_HOME to an absolute installation directory to override
${XDG_DATA_HOME:-$HOME/.local/share}/ochrance/zig-0.15.2.
After installation, add the printed bin directory to PATH for this shell.
HELP
}
if [[ $# -gt 0 ]]; then
  if [[ $# == 1 && ( $1 == --help || $1 == -h ) ]]; then usage; exit 0; fi
  usage >&2
  exit 2
fi

case "$(uname -s)/$(uname -m)" in
  Linux/x86_64|Linux/aarch64) ;;
  *) echo 'ERROR: only Linux x86_64/aarch64 wheels are pinned.' >&2; exit 1 ;;
esac

root="${OCHRANCE_ZIG_HOME:-${XDG_DATA_HOME:-$HOME/.local/share}/ochrance/zig-0.15.2}"
case "$root" in
  /*) ;;
  *) echo 'ERROR: OCHRANCE_ZIG_HOME must be an absolute path.' >&2; exit 1 ;;
esac
if [[ "$root" == / || "$root" == *$'\n'* ]]; then
  echo 'ERROR: unsafe installation path.' >&2
  exit 1
fi
script_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

# A completed installation needs no network (or global Python) on restart.
if [[ -x "$root/bin/zig" ]] && [[ "$("$root/bin/zig" version)" == 0.15.2 ]]; then
  printf 'Zig 0.15.2 already installed. Activate with:\nexport PATH=%q:"$PATH"\n' "$root/bin"
  exit 0
fi
command -v python3 >/dev/null || { echo 'ERROR: python3 is required.' >&2; exit 1; }
mkdir -p -- "$root"
# Do not let concurrent session setup processes mutate the same venv.
if ! mkdir -- "$root/.install-lock" 2>/dev/null; then
  echo "ERROR: installation locked at $root/.install-lock (another install or interrupted process)." >&2
  exit 1
fi
trap 'rmdir -- "$root/.install-lock"' EXIT

python3 -m venv "$root/venv"
"$root/venv/bin/python" -m pip --isolated install \
  --index-url https://pypi.org/simple \
  --require-hashes --only-binary=:all: --no-deps \
  -r "$script_dir/zig-requirements.txt"
[[ "$("$root/venv/bin/python" -m ziglang version)" == 0.15.2 ]] || {
  echo 'ERROR: installed Zig version does not match 0.15.2.' >&2
  exit 1
}
mkdir -p -- "$root/bin"
# Resolve relative to the shim; do not embed or evaluate the installation path.
cat > "$root/bin/zig" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
exec "$root/venv/bin/python" -m ziglang "$@"
SH
chmod +x "$root/bin/zig"
printf 'Installed Zig 0.15.2. Activate with:\nexport PATH=%q:"$PATH"\n' "$root/bin"
