;;; forgejo-test-tl.el --- Tests for forgejo-tl  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the fast tabulated-list renderer, especially point
;; restoration when the saved entry has been filtered out.

;;; Code:

(require 'forgejo-test-helper)
(require 'forgejo-tl)
(require 'forgejo-notification)

(defun forgejo-test-tl--setup (entries)
  "Install ENTRIES in the current buffer with a single Name column."
  (tabulated-list-mode)
  (setq tabulated-list-format [("Name" 20 nil)]
        tabulated-list-entries entries
        tabulated-list-padding 0)
  (tabulated-list-init-header)
  (forgejo-tl-print))

(ert-deftest forgejo-test-tl-remember-pos-found ()
  "Restore point to the saved entry when it still exists."
  (with-temp-buffer
    (forgejo-test-tl--setup
     '((1 ["one"]) (2 ["two"]) (3 ["three"])))
    (goto-char (point-min))
    (forward-line 1)
    (should (equal (tabulated-list-get-id) 2))
    (forgejo-tl-print t)
    (should (equal (tabulated-list-get-id) 2))))

(ert-deftest forgejo-test-tl-remember-pos-filtered-out ()
  "Fall back to the saved line when the saved entry is gone."
  (with-temp-buffer
    (forgejo-test-tl--setup
     '((1 ["one"]) (2 ["two"]) (3 ["three"]) (4 ["four"])))
    (goto-char (point-min))
    (forward-line 1)
    (should (equal (tabulated-list-get-id) 2))
    (setq tabulated-list-entries
          '((1 ["one"]) (3 ["three"]) (4 ["four"])))
    (forgejo-tl-print t)
    ;; Entry 2 is gone; cursor should stay on line 2, now showing 3.
    (should (equal (line-number-at-pos) 2))
    (should (equal (tabulated-list-get-id) 3))))

(ert-deftest forgejo-test-tl-remember-pos-no-id ()
  "Without REMEMBER-POS, point goes to point-min."
  (with-temp-buffer
    (forgejo-test-tl--setup
     '((1 ["one"]) (2 ["two"]) (3 ["three"])))
    (goto-char (point-min))
    (forward-line 2)
    (forgejo-tl-print)
    (should (equal (point) (point-min)))))

;;; Notification browser

(defun forgejo-test-tl--notification (pull-request)
  "Render a notification row with PULL-REQUEST metadata."
  (forgejo-notification-list-mode)
  (setq tabulated-list-entries
        (forgejo-notification--build-entries
         (list (forgejo-test-issue
                `((notification_thread_id . 123)
                  (notification_owner . "alice")
                  (notification_repo . "project")
                  (pull_request . ,pull-request))))))
  (forgejo-tl-print)
  (should (equal (tabulated-list-get-id) 123)))

(ert-deftest forgejo-test-tl-notification-browse-type ()
  "Browse real issue and pull rows using their subject type."
  (dolist (case '((nil . "issues") (t . "pulls")
                  (((merged . :false)) . "pulls")))
    (with-temp-buffer
      (forgejo-test-tl--notification (car case))
      (setq forgejo-notification--host "forge.example"
            forgejo-notification--host-url "https://forge.example")
      (let (urls)
        (cl-letf (((symbol-function 'browse-url)
                   (lambda (url &rest _) (push url urls))))
          (call-interactively #'forgejo-notification-browse-at-point))
        (should (equal urls
                       (list (format "https://forge.example/alice/project/%s/42"
                                     (cdr case)))))))))

(ert-deftest forgejo-test-tl-notification-browse-origin ()
  "Keep the buffer URL, falling back to configured URL or HTTPS."
  (dolist (pull-request '(nil t))
    (dolist (case '(("http://forge.example:3000" nil
                     "http://forge.example:3000")
                    ("https://forge.example:8443/forgejo"
                     (("http://forge.example:3000"))
                     "https://forge.example:8443/forgejo")
                    (nil (("http://forge.example:3000"))
                     "http://forge.example:3000")
                    (nil nil "https://forge.example")))
      (with-temp-buffer
        (forgejo-test-tl--notification pull-request)
        (setq forgejo-notification--host "forge.example"
              forgejo-notification--host-url (car case))
        (let ((forgejo-hosts (cadr case))
              urls)
          (cl-letf (((symbol-function 'browse-url)
                     (lambda (url &rest _) (push url urls))))
            (call-interactively #'forgejo-notification-browse-at-point))
          (should (equal urls
                         (list (format "%s/alice/project/%s/42"
                                       (nth 2 case)
                                       (if pull-request "pulls" "issues"))))))))))

(ert-deftest forgejo-test-tl-notification-browse-no-subject ()
  "Do nothing without a selected row or a valid full reference."
  (with-temp-buffer
    (forgejo-test-tl--notification nil)
    (cl-letf (((symbol-function 'browse-url)
               (lambda (&rest _) (ert-fail "Unexpected browser call"))))
      (goto-char (point-max))
      (should-not (tabulated-list-get-entry))
      (should-not (call-interactively #'forgejo-notification-browse-at-point))
      (goto-char (point-min))
      (let ((ref (aref (tabulated-list-get-entry) 1)))
        (dolist (full '(nil "alice/project#invalid"))
          (put-text-property 0 (length ref) 'forgejo-full-ref full ref)
          (should-not
           (call-interactively #'forgejo-notification-browse-at-point)))))))

(ert-deftest forgejo-test-tl-notification-browse-error ()
  "Propagate browser errors without consuming the selected notification."
  (with-temp-buffer
    (forgejo-test-tl--notification t)
    (setq forgejo-notification--host "forge.example"
          forgejo-notification--host-url "https://forge.example")
    (let ((before (buffer-string)))
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (&rest _) (user-error "Browser unavailable"))))
        (should (equal
                 (should-error
                  (call-interactively #'forgejo-notification-browse-at-point)
                  :type 'user-error)
                 '(user-error "Browser unavailable"))))
      (should (equal (buffer-string) before))
      (should (equal (tabulated-list-get-id) 123)))))

(provide 'forgejo-test-tl)
;;; forgejo-test-tl.el ends here
