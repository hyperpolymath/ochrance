# PROOF-NEEDS.md — ochrance

## Current State

- **src/abi/*.idr**: YES — `Ochrance/` subdirectory
- **Dangerous patterns**: 0 (1 hit is in echidnabot.scm documentation)
- **LOC**: ~6,300
- **ABI layer**: Idris2 ABI present in Ochrance subdirectory

## What Needs Proving

| Component | What | Why |
|-----------|------|-----|
| Protection policy evaluation | Policy decisions are deterministic and total | Inconsistent protection decisions leave gaps |
| Guard state machine | State transitions cover all edge cases | Missed transitions leave system unprotected |
| Alert classification | Threat classification is sound (no false negatives for critical threats) | Missing critical alerts is a security failure |
| Audit trail integrity | Audit log entries are append-only and tamper-evident | Compromised audit trail hides attacks |

## Recommended Prover

**Idris2** — ABI layer present. Protection policy evaluation and guard state machines are natural fits for dependent types with exhaustive pattern matching.

## Priority

**MEDIUM** — Security guardian tool. Policy evaluation correctness is the highest-value proof target. False negatives in threat classification are the most dangerous failure mode.
