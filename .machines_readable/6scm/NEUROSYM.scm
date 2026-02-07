;; SPDX-License-Identifier: PMPL-1.0-or-later
;; NEUROSYM.scm - Neurosymbolic integration config for ochrance

(define neurosym-config
  `((version . "1.0.0")
    (symbolic-layer
      ((type . "idris2-dependent-types")
       (reasoning . "dependent-type-checking")
       (verification . "totality-proofs")
       (framework . "VerifiedSubsystem")
       (error-taxonomy . "q/p/z")))
    (neural-layer
      ((type . "echidna-neural-proof-synthesis")
       (prover . "echidna")
       (ffi . "libechidna.so")
       (synthesis . "proof-search")
       (multi-prover . true)))
    (integration
      ((direction . "bidirectional")
       (idris2-to-echidna . "proof-obligations")
       (echidna-to-idris2 . "synthesized-proofs")
       (corpus . "200-idris2-examples")))))
