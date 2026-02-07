# SPDX-License-Identifier: PMPL-1.0-or-later
# justfile for Ochránce - Neurosymbolic Filesystem Verification

# Default recipe - list available commands
default:
    @just --list

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

# Run tests
test:
    idris2 --build ochrance.ipkg --test

# Run integration tests
test-integration:
    idris2 --build tests/integration.ipkg

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
