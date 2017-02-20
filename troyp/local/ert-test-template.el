;;;  -*- lexical-binding: t -*-

(require 'pkg)
(require 'ert)

(cl-macrolet ((should-equal
               (expr keyword result)
               (progn
                 (unless (eq keyword :result) (error "expected :result"))
                 `(should (equal ,expr ,result)))))

  (ert-deftest test-pkg-unit-tests ()
    "Unit tests for pkg."

    )

  )

(defun pkg---test-all ()
  (interactive)
  (ert-run-tests-batch "^test-pkg" ))
