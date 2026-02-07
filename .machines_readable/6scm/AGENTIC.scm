;; SPDX-License-Identifier: PMPL-1.0-or-later
;; AGENTIC.scm - AI agent interaction patterns for ochrance

(define agentic-config
  `((version . "1.0.0")
    (claude-code
      ((model . "claude-opus-4-5-20251101")
       (tools . ("read" "edit" "bash" "grep" "glob"))
       (permissions . "read-all")))
    (patterns
      ((code-review . "totality-focused")
       (refactoring . "proof-preserving")
       (testing . "property-based")))
    (constraints
      ((languages . ("idris2" "zig" "rust" "julia"))
       (banned . ("typescript" "go" "python" "makefile"))
       (idris2-requirements . ("%default total" "structural-recursion" "explicit-exports"))
       (totality . "all-functions-must-be-total")))))
