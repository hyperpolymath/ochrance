# CLAUDE.md - AI Assistant Instructions

## Machine-Readable Artefacts

This repo follows the hyperpolymath standard. See `https://github.com/hyperpolymath/standards` for the canonical 6-file `.machine_readable/` layout (STATE/META/ECOSYSTEM/AGENTIC/NEUROSYM/PLAYBOOK in A2ML format).

---

## Language Policy (Hyperpolymath Standard)

The full policy is canonical in `hyperpolymath/standards`. Key points:

- **AffineScript** - Primary application code (compiles to typed-wasm)
- **Deno** - Runtime & package management (replaces Node/npm/bun)
- **Rust/SPARK** - Performance-critical, systems, WASM, CLI tools
- **Zig** - FFI layer, memory-safe systems
- **Idris2** - Formal verification
- **Agda** - Formal verification (foundational)
- **Tauri 2.0+** / **Dioxus** - Mobile apps
- **Gleam** / **Elixir** - Backend services
- **Haskell** - Type-heavy tools
- **Bash/POSIX Shell** - Scripts, automation (keep minimal)
- **JavaScript** - Only where AffineScript cannot
- **Nickel** - Configuration language
- **A2ML** - State/meta files

### BANNED
- TypeScript (use AffineScript)
- Node.js (use Deno)
- Go (use Rust)
- Python (general, except SaltStack)
- Java/Kotlin/Swift (use Rust/Tauri/Dioxus)

### Documentation Format
- All docs `.adoc` (AsciiDoc) except GitHub-required files (SECURITY.md, CONTRIBUTING.md, CODE_OF_CONDUCT.md, CHANGELOG.md).

