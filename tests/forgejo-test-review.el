;;; forgejo-test-review.el --- Tests for forgejo-review  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for Forgejo pull review operations.

;;; Code:

(require 'forgejo-test-helper)
(require 'forgejo-review)

;;; Group 1: Diff approval

(ert-deftest forgejo-test-review-diff-approve-posts-review-approval ()
  "Diff approval posts an approval review without line comments."
  (let (posted-host posted-endpoint posted-params posted-body)
    (cl-letf (((symbol-function 'forgejo-api-post)
               (lambda (host endpoint params body callback)
                 (setq posted-host host
                       posted-endpoint endpoint
                       posted-params params
                       posted-body body)
                 (funcall callback nil nil))))
      (with-temp-buffer
        (setq-local forgejo-repo--host "https://codeberg.org")
        (setq-local forgejo-repo--owner "OWNER")
        (setq-local forgejo-repo--name "REPO")
        (setq-local forgejo-diff--pr-number 123)
        (forgejo-review-diff-approve)))
    (should (string= posted-host "https://codeberg.org"))
    (should (string= posted-endpoint "repos/OWNER/REPO/pulls/123/reviews"))
    (should-not posted-params)
    (should (equal posted-body '((event . "APPROVED"))))
    (should-not (assq 'comments posted-body))))

(ert-deftest forgejo-test-review-diff-approve-requires-full-context ()
  "Diff approval requires repo context and an associated PR number."
  (with-temp-buffer
    (setq-local forgejo-repo--owner "OWNER")
    (setq-local forgejo-repo--name "REPO")
    (setq-local forgejo-diff--pr-number 123)
    (should-error (forgejo-review-diff-approve) :type 'user-error)))

(ert-deftest forgejo-test-review-diff-comment-requires-full-context ()
  "Diff comments require repo context and an associated PR number."
  (with-temp-buffer
    (setq-local forgejo-repo--host "https://codeberg.org")
    (setq-local forgejo-repo--name "REPO")
    (setq-local forgejo-diff--pr-number 123)
    (should-error (forgejo-review-diff-comment) :type 'user-error)))

;;; Group 2: Composer submission and cancellation

(defun forgejo-test-review--compose (type key text expected-body)
  "Submit TYPE via composer KEY with TEXT and expect EXPECTED-BODY.
A nil EXPECTED-BODY means no request or success action is allowed.
Only recursive-edit entry/exit is simulated; use the real compose binding."
  (let (posts refreshes messages composer)
    (save-window-excursion
      (with-temp-buffer
        (setq-local forgejo-repo--host "https://forge.example"
                    forgejo-repo--owner "owner"
                    forgejo-repo--name "repo"
                    forgejo-view--data '((number . 7))
                    forgejo-post-action-functions
                    (list (lambda () (push t refreshes))))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) type))
                  ((symbol-function 'recursive-edit)
                   (lambda ()
                     (setq composer (current-buffer))
                     (should forgejo-compose-mode)
                     (insert text)
                     (should (eq (key-binding (kbd key))
                                 (if expected-body
                                     'forgejo-compose-done
                                   'forgejo-compose-abort)))
                     (catch 'forgejo-test-compose-exit
                       (call-interactively (key-binding (kbd key))))))
                  ((symbol-function 'exit-recursive-edit)
                   (lambda () (throw 'forgejo-test-compose-exit nil)))
                  ((symbol-function 'forgejo-api-post)
                   (lambda (&rest args) (push args posts)))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) messages))))
          (call-interactively #'forgejo-review-submit)
          (should (bufferp composer))
          (should-not (buffer-live-p composer))
          (should-not refreshes)
          (should-not messages)
          (if expected-body
              (progn
                (should (= (length posts) 1))
                (should (equal (seq-take (car posts) 4)
                               (list "https://forge.example"
                                     "repos/owner/repo/pulls/7/reviews"
                                     nil expected-body)))
                (funcall (nth 4 (car posts)) nil nil)
                (should (equal refreshes '(t)))
                (should (= (length messages) 1))
                (should (string-prefix-p "Review submitted:" (car messages))))
            (should-not posts)))))))

(ert-deftest forgejo-test-review-submit-cancel-approval ()
  "Canceling an approval dispatches neither a review nor a success action."
  (forgejo-test-review--compose "approve" "C-c C-k" "Looks good" nil))

(ert-deftest forgejo-test-review-submit-cancel-comment ()
  "Canceling a comment dispatches neither a review nor a success action."
  (forgejo-test-review--compose "comment" "C-c C-k" "Needs work" nil))

(ert-deftest forgejo-test-review-submit-empty-approval ()
  "Explicitly submitting an empty approval still posts an approval."
  (forgejo-test-review--compose "approve" "C-c C-c" ""
                                '((event . "APPROVED"))))

(ert-deftest forgejo-test-review-submit-authored-body ()
  "Submitted approval and comment bodies retain their authored text."
  (dolist (choice '(("approve" . "APPROVED") ("comment" . "COMMENT")))
    (forgejo-test-review--compose
     (car choice) "C-c C-c" "  Review text\n"
     `((event . ,(cdr choice)) (body . "  Review text\n")))))

(provide 'forgejo-test-review)
;;; forgejo-test-review.el ends here
