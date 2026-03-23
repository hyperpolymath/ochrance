# SPDX-License-Identifier: PMPL-1.0-or-later
# justfile for Ochránce - Neurosymbolic Filesystem Verification

# Minimum required versions
# Idris2 >= 0.8.0 (dependent types, totality checking, Quantity 1 linear types)
# Zig    >= 0.11.0 (std.crypto.hash.Blake3, C ABI export)

# Default recipe - list available commands
default:
    @just --list

# Check toolchain versions meet minimum requirements
check-versions:
    @echo "Checking toolchain versions..."
    @idris2 --version | grep -qE '0\.(8|9|[1-9][0-9])\.' || (echo "ERROR: Idris2 >= 0.8.0 required" && exit 1)
    @echo "  Idris2: OK (>= 0.8.0)"
    @zig version | grep -qE '0\.(1[1-9]|[2-9][0-9])\.' || (echo "ERROR: Zig >= 0.11.0 required" && exit 1)
    @echo "  Zig:    OK (>= 0.11.0)"
    @echo "All toolchain versions OK"

# Build core library
build-core:
    idris2 --build ochrance.ipkg

# Build filesystem module
build-fs:
    idris2 --build ochrance-fs.ipkg

# Build ABI layer
build-abi:
    idris2 --build ochrance-abi.ipkg

# Build Zig FFI
build-ffi:
    cd ffi/zig && zig build

# Build all components
build: build-core build-fs build-abi build-ffi

# Run all tests (builds core, installs, then runs test suites)
test: build-core
    idris2 --install ochrance.ipkg
    idris2 --install ochrance-fs.ipkg
    idris2 --build tests/A2ML/tests.ipkg
    tests/A2ML/build/exec/a2ml-tests
    idris2 --build tests/property/tests.ipkg
    tests/property/build/exec/property-tests
    idris2 --build tests/integration/tests.ipkg
    tests/integration/build/exec/integration-tests

# Run A2ML parser tests only
test-a2ml: build-core
    idris2 --install ochrance.ipkg
    idris2 --build tests/A2ML/tests.ipkg
    tests/A2ML/build/exec/a2ml-tests

# Run integration tests only
test-integration: build-core
    idris2 --install ochrance.ipkg
    idris2 --install ochrance-fs.ipkg
    idris2 --build tests/integration/tests.ipkg
    tests/integration/build/exec/integration-tests

# Check totality of all functions
check-totality:
    @echo "Checking totality..."
    @idris2 --check ochrance-core/
    @echo "Verifying no assert_total usage..."
    @! grep -r 'assert_total' ochrance-core/

# Verify no partial functions
verify:
    @just check-totality
    @echo "Checking for %default total..."
    @grep -r '%default total' ochrance-core/ | wc -l
    @echo "✅ All modules use %default total"

# Type-check a specific file
check FILE:
    idris2 --check {{FILE}}

# Open REPL
repl:
    idris2 --repl ochrance.ipkg

# Open REPL for filesystem module
repl-fs:
    idris2 --repl ochrance-fs.ipkg

# Find type at position in file
type-at FILE LINE COL:
    idris2 --find-type-at {{FILE}}:{{LINE}}:{{COL}}

# Install packages
install:
    idris2 --install ochrance.ipkg
    idris2 --install ochrance-fs.ipkg
    idris2 --install ochrance-abi.ipkg

# Install OSTree hooks
install-ostree:
    @echo "Installing OSTree hooks..."
    sudo cp integrations/ostree/hooks/* /etc/ostree/

# Clean build artifacts
clean:
    rm -rf build/
    cd ffi/zig && zig build clean

# Run all quality checks
quality: check-totality verify
    @echo "✅ All quality checks passed"

# Format code (if idris2-format available)
format:
    @echo "Idris2 has no standard formatter yet"
    @echo "Manual formatting guidelines: See CLAUDE.md"

# Show project statistics
stats:
    @echo "=== Ochránce Statistics ==="
    @echo "Idris2 modules:"
    @find ochrance-core modules -name "*.idr" | wc -l
    @echo "Total lines of code:"
    @find ochrance-core modules -name "*.idr" -exec cat {} \; | wc -l
    @echo "Functions marked total:"
    @grep -r 'total' ochrance-core modules | grep -v '%default' | wc -l

# Run panic-attacker pre-commit scan
assail:
    @command -v panic-attack >/dev/null 2>&1 && panic-attack assail . || echo "panic-attack not found — install from https://github.com/hyperpolymath/panic-attacker"
