;; SPDX-License-Identifier: PMPL-1.0-or-later
;; PLAYBOOK.scm - Operational runbook for ochrance

(define playbook
  `((version . "1.0.0")
    (procedures
      ((build . (("core" . "idris2 --build ochrance.ipkg")
                 ("filesystem" . "idris2 --build ochrance-fs.ipkg")
                 ("abi" . "idris2 --build ochrance-abi.ipkg")
                 ("zig-ffi" . "cd ffi/zig && zig build")))
       (test . (("parser" . "idris2 --build ochrance.ipkg --test")
                ("totality" . "idris2 --check ochrance-core/")
                ("integration" . "idris2 --build tests/integration.ipkg")))
       (verify . (("totality" . "grep -r '%default total' ochrance-core/")
                  ("no-assert" . "! grep -r 'assert_total' ochrance-core/")
                  ("coverage" . "idris2 --coverage")))
       (debug . (("repl" . "idris2 --repl ochrance.ipkg")
                 ("check-file" . "idris2 --check FILE.idr")
                 ("type-at" . "idris2 --find-type-at")))
       (deploy . (("install" . "idris2 --install ochrance.ipkg")
                  ("ostree-hook" . "cp integrations/ostree/hooks/* /etc/ostree/")))))
    (alerts
      ((totality-violation . "Check for assert_total or missing %default total")
       (ffi-failure . "Verify libechidna.so is built and in LD_LIBRARY_PATH")
       (proof-timeout . "Increase fuel parameter in structural recursion")))
    (contacts
      ((maintainer . "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>")
       (repo . "https://github.com/hyperpolymath/ochrance")))))
