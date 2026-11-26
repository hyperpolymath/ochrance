#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Opt-in, user-local Zig installation for restricted Linux web sessions.
set -euo pipefail

usage() {
  cat <<'HELP'
Usage: bash .devcontainer/install-zig.sh
Install Zig 0.15.2 from the official SHA-256 pinned release tarball.
Requires Linux x86_64/aarch64, bash, curl, tar, xz and sha256sum, and
HTTPS access to ziglang.org (or the mirror in OCHRANCE_ZIG_MIRROR).
No sudo, no Python, no shell-profile edits, and no automatic activation.
Set OCHRANCE_ZIG_HOME to an absolute installation directory to override
${XDG_DATA_HOME:-$HOME/.local/share}/ochrance/zig-0.15.2.
Set OCHRANCE_ZIG_MIRROR to an https:// base URL that serves the same
file names, if ziglang.org is unreachable; the checksum is still enforced.
After installation, add the printed bin directory to PATH for this shell.
HELP
}
if [[ $# -gt 0 ]]; then
  if [[ $# == 1 && ( $1 == --help || $1 == -h ) ]]; then usage; exit 0; fi
  usage >&2
  exit 2
fi

version=0.15.2
# SHA-256 of the official tarballs, from https://ziglang.org/download/index.json
case "$(uname -s)/$(uname -m)" in
  Linux/x86_64)
    arch=x86_64
    sha256=02aa270f183da276e5b5920b1dac44a63f1a49e55050ebde3aecc9eb82f93239 ;;
  Linux/aarch64)
    arch=aarch64
    sha256=958ed7d1e00d0ea76590d27666efbf7a932281b3d7ba0c6b01b0ff26498f667f ;;
  *) echo 'ERROR: only Linux x86_64/aarch64 tarballs are pinned.' >&2; exit 1 ;;
esac
name="zig-$arch-linux-$version"

root="${OCHRANCE_ZIG_HOME:-${XDG_DATA_HOME:-$HOME/.local/share}/ochrance/zig-$version}"
case "$root" in
  /*) ;;
  *) echo 'ERROR: OCHRANCE_ZIG_HOME must be an absolute path.' >&2; exit 1 ;;
esac
if [[ "$root" == / || "$root" == *$'\n'* ]]; then
  echo 'ERROR: unsafe installation path.' >&2
  exit 1
fi

base="${OCHRANCE_ZIG_MIRROR:-https://ziglang.org/download/$version}"
case "$base" in
  https://*) ;;
  *) echo 'ERROR: OCHRANCE_ZIG_MIRROR must be an https:// URL.' >&2; exit 1 ;;
esac
url="${base%/}/$name.tar.xz"

# A completed installation needs no network on restart.
if [[ -x "$root/bin/zig" ]] && [[ "$("$root/bin/zig" version)" == "$version" ]]; then
  printf 'Zig %s already installed. Activate with:\nexport PATH=%q:"$PATH"\n' "$version" "$root/bin"
  exit 0
fi
for tool in curl tar xz sha256sum; do
  command -v "$tool" >/dev/null || { echo "ERROR: $tool is required." >&2; exit 1; }
done
mkdir -p -- "$root"
# Do not let concurrent session setup processes mutate the same prefix.
if ! mkdir -- "$root/.install-lock" 2>/dev/null; then
  echo "ERROR: installation locked at $root/.install-lock (another install or interrupted process)." >&2
  exit 1
fi
stage="$root/.install-lock/stage"
trap 'rm -rf -- "$root/.install-lock"' EXIT
mkdir -- "$stage"

curl --proto '=https' --tlsv1.2 -fsSL --retry 2 -o "$stage/$name.tar.xz" -- "$url"
got="$(sha256sum -- "$stage/$name.tar.xz")"
got="${got%% *}"
if [[ "$got" != "$sha256" ]]; then
  echo "ERROR: checksum mismatch for $name.tar.xz (expected $sha256, got $got)." >&2
  exit 1
fi
tar -xJf "$stage/$name.tar.xz" -C "$stage"
[[ "$("$stage/$name/zig" version)" == "$version" ]] || {
  echo "ERROR: installed Zig version does not match $version." >&2
  exit 1
}

# Replace only what this installer owns: dist/ and bin/zig.
rm -rf -- "$root/dist"
mv -- "$stage/$name" "$root/dist"
mkdir -p -- "$root/bin"
# Resolve relative to the shim; do not embed or evaluate the installation path.
cat > "$root/bin/zig" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
exec "$root/dist/zig" "$@"
SH
chmod +x "$root/bin/zig"
printf 'Installed Zig %s. Activate with:\nexport PATH=%q:"$PATH"\n' "$version" "$root/bin"
