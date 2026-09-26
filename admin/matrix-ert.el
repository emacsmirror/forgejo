;;; matrix-ert.el --- Batch completion evidence -*- lexical-binding: t; -*-

;; Loaded by the runner after requiring one test library.
(require 'ert)
(require 'json)
(require 'sqlite)

(unless (and (sqlite-available-p) (libxml-available-p))
  (error "Forgejo tests require SQLite and libxml support"))

(let* ((selector (car (read-from-string (getenv "FORGEJO_TEST_SELECTOR"))))
       (names (mapcar (lambda (test) (symbol-name (ert-test-name test)))
                      (ert-select-tests selector t)))
       (stats (ert-run-tests-batch selector)))
  (with-temp-file (getenv "FORGEJO_MATRIX_RECEIPT")
    (insert (json-encode
             `((suite . ,(getenv "FORGEJO_TEST_SUITE"))
               (names . ,(vconcat names))
               (sqlite . t) (libxml . t)
               (total . ,(ert-stats-total stats))
               (completed . ,(ert-stats-completed stats))
               (expected . ,(ert-stats-completed-expected stats))
               (unexpected . ,(ert-stats-completed-unexpected stats))
               (skipped . ,(ert-stats-skipped stats)))))))
;;; matrix-ert.el ends here
