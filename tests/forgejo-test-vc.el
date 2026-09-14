;;; forgejo-test-vc.el --- Tests for forgejo-vc  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for AGit-Flow helpers: refspec building, description
;; encoding, and push option construction.

;;; Code:

(require 'forgejo-test-helper)
(require 'forgejo-vc)
(require 'forgejo-repo)

;;; Group 1: Refspec building

(ert-deftest forgejo-test-vc-refspec ()
  "Build a correct AGit-Flow refspec."
  (should (string= (forgejo-vc--refspec "HEAD" "main" "fix-login")
                   "HEAD:refs/for/main/fix-login")))

(ert-deftest forgejo-test-vc-refspec-feature-branch ()
  "Refspec works with feature branch topics."
  (should (string= (forgejo-vc--refspec "HEAD" "develop" "feature/auth")
                   "HEAD:refs/for/develop/feature/auth")))

;;; Group 2: Description encoding

(ert-deftest forgejo-test-vc-encode-description ()
  "Base64-encode description with {base64} prefix."
  (let ((result (forgejo-vc--encode-description "Hello World")))
    (should (string-prefix-p "{base64}" result))
    (should (string= (decode-coding-string
                      (base64-decode-string
                       (substring result (length "{base64}")))
                      'utf-8)
                     "Hello World"))))

(ert-deftest forgejo-test-vc-encode-description-unicode ()
  "UTF-8 text roundtrips through encoding."
  (let* ((text "Unicode: \u03b1\u03b2\u03b3")
         (result (forgejo-vc--encode-description text))
         (decoded (decode-coding-string
                   (base64-decode-string
                    (substring result (length "{base64}")))
                   'utf-8)))
    (should (string= decoded text))))

;;; Group 3: Push options

(ert-deftest forgejo-test-vc-push-options ()
  "Push options include title and encoded description."
  (let ((opts (forgejo-vc--push-options "My PR" "Description text")))
    (should (= (length opts) 4))
    (should (string= (nth 0 opts) "-o"))
    (should (string= (nth 1 opts) "title=My PR"))
    (should (string= (nth 2 opts) "-o"))
    (should (string-prefix-p "description={base64}" (nth 3 opts)))))

;;; Group 4: Remote detection

(defun forgejo-test-vc--mock-process-file (url)
  "Return a mock `process-file' that returns URL for get-url, \"origin\" for remote."
  (lambda (_prog _infile _dest _display &rest args)
    (cond
     ((member "get-url" args) (insert url))
     ((equal args '("remote")) (insert "origin")))
    0))

(ert-deftest forgejo-test-vc-repo-from-remote-https ()
  "Parse HTTPS remote into (HOST OWNER REPO REMOTE)."
  (let ((forgejo-hosts '(("https://codeberg.org"))))
    (cl-letf (((symbol-function 'process-file)
               (forgejo-test-vc--mock-process-file
                "https://codeberg.org/thanos/forgejo.git")))
      (should (equal (forgejo-vc--repo-from-remote)
                     '("https://codeberg.org" "thanos" "forgejo" "origin"))))))

(ert-deftest forgejo-test-vc-repo-from-remote-ssh ()
  "Parse SSH remote into (HOST OWNER REPO REMOTE)."
  (let ((forgejo-hosts '(("https://codeberg.org"))))
    (cl-letf (((symbol-function 'process-file)
               (forgejo-test-vc--mock-process-file
                "ssh://git@codeberg.org/thanos/forgejo.git")))
      (should (equal (forgejo-vc--repo-from-remote)
                     '("https://codeberg.org" "thanos" "forgejo" "origin"))))))

(ert-deftest forgejo-test-vc-repo-from-remote-scp ()
  "Parse SCP-style remote into (HOST OWNER REPO REMOTE)."
  (let ((forgejo-hosts '(("https://codeberg.org"))))
    (cl-letf (((symbol-function 'process-file)
               (forgejo-test-vc--mock-process-file
                "git@codeberg.org:thanos/forgejo")))
      (should (equal (forgejo-vc--repo-from-remote)
                     '("https://codeberg.org" "thanos" "forgejo" "origin"))))))

(ert-deftest forgejo-test-vc-repo-from-remote-selfhosted ()
  "Parse HTTPS remote from a configured self-hosted instance."
  (let ((forgejo-hosts '(("https://git.example.com"))))
    (cl-letf (((symbol-function 'process-file)
               (forgejo-test-vc--mock-process-file
                "https://git.example.com/org/project.git")))
      (should (equal (forgejo-vc--repo-from-remote)
                     '("https://git.example.com" "org" "project" "origin"))))))

(ert-deftest forgejo-test-vc-repo-from-remote-ignores-unconfigured ()
  "Remotes whose host is not in `forgejo-hosts' are ignored."
  (let ((forgejo-hosts '(("https://codeberg.org"))))
    (cl-letf (((symbol-function 'process-file)
               (forgejo-test-vc--mock-process-file
                "https://gitlab.com/org/project.git")))
      (should-not (forgejo-vc--repo-from-remote)))))

;;; Group 5: Target default resolution

(ert-deftest forgejo-test-vc-target-default-prefers-default-branch ()
  "Repo default branch wins over BRANCH's upstream."
  (cl-letf (((symbol-function 'forgejo-vc--remembered-target)
             (lambda (_branch) nil))
            ((symbol-function 'forgejo-vc--default-target)
             (lambda () "origin/master"))
            ((symbol-function 'forgejo-vc--upstream-branch)
             (lambda (_branch) "guixotic/topic")))
    (should (string= (forgejo-vc--target-default "topic") "origin/master"))))

(ert-deftest forgejo-test-vc-target-default-falls-back-to-upstream ()
  "Upstream is used when no repo default branch is known."
  (cl-letf (((symbol-function 'forgejo-vc--remembered-target)
             (lambda (_branch) nil))
            ((symbol-function 'forgejo-vc--default-target)
             (lambda () nil))
            ((symbol-function 'forgejo-vc--upstream-branch)
             (lambda (_branch) "origin/main")))
    (should (string= (forgejo-vc--target-default "topic") "origin/main"))))

(ert-deftest forgejo-test-vc-target-default-prefers-remembered ()
  "A remembered target wins over the repo default branch."
  (cl-letf (((symbol-function 'forgejo-vc--remembered-target)
             (lambda (_branch) "origin/devel"))
            ((symbol-function 'forgejo-vc--default-target)
             (lambda () "origin/master")))
    (should (string= (forgejo-vc--target-default "topic") "origin/devel"))))

;;; Group 6: Submit persists target

(ert-deftest forgejo-test-vc-submit-remembers-target ()
  "Submitting persists the chosen target for the current branch."
  (let (remembered)
    (cl-letf (((symbol-function 'forgejo-vc--remote-url) (lambda (_r) nil))
              ((symbol-function 'forgejo-vc--git-push) (lambda (&rest _) nil))
              ((symbol-function 'vc-git-branches) (lambda () '("topic")))
              ((symbol-function 'forgejo-vc--remember-target)
               (lambda (branch target) (setq remembered (cons branch target)))))
      (forgejo-vc-submit "origin" "topic" "master" t)
      (should (equal remembered '("topic" . "origin/master"))))))

(ert-deftest forgejo-test-vc-submit-remembers-target-normal-path ()
  "The non-force submit path persists the target after the push."
  (let (remembered)
    (cl-letf (((symbol-function 'forgejo-vc--remote-url) (lambda (_r) nil))
              ((symbol-function 'forgejo-vc--git-push) (lambda (&rest _) nil))
              ((symbol-function 'vc-git-branches) (lambda () '("topic")))
              ((symbol-function 'forgejo-vc--autofill-defaults)
               (lambda (_u) (cons "Title" "")))
              ((symbol-function 'forgejo-vc--find-pr-template) (lambda (_r) nil))
              ((symbol-function 'read-string) (lambda (&rest _) "PR title"))
              ((symbol-function 'forgejo-utils-read-body) (lambda (&rest _) "body"))
              ((symbol-function 'forgejo-vc--remember-target)
               (lambda (branch target) (setq remembered (cons branch target)))))
      (forgejo-vc-submit "origin" "topic" "master" nil)
      (should (equal remembered '("topic" . "origin/master"))))))

(ert-deftest forgejo-test-vc-submit-skips-remember-on-cancel ()
  "Aborting before the push does not persist a target."
  (let (called)
    (cl-letf (((symbol-function 'forgejo-vc--remote-url) (lambda (_r) nil))
              ((symbol-function 'forgejo-vc--git-push) (lambda (&rest _) nil))
              ((symbol-function 'vc-git-branches) (lambda () '("topic")))
              ((symbol-function 'forgejo-vc--autofill-defaults)
               (lambda (_u) (cons "" "")))
              ((symbol-function 'forgejo-vc--find-pr-template) (lambda (_r) nil))
              ((symbol-function 'read-string) (lambda (&rest _) ""))
              ((symbol-function 'forgejo-vc--remember-target)
               (lambda (&rest _) (setq called t))))
      (should-error (forgejo-vc-submit "origin" "topic" "master" nil)
                    :type 'user-error)
      (should-not called))))

;;; Group 7: Configured web endpoints and SSH transport

(defun forgejo-test-vc--configured-endpoint (host-url remote-url)
  "Exercise VC web operations for HOST-URL beside SSH REMOTE-URL."
  (ert-with-temp-directory dir
    (let ((default-directory (file-name-as-directory dir))
          (forgejo-hosts (list (list host-url)))
          (forgejo-vc--selected-remotes (make-hash-table :test 'equal))
          (forgejo-vc--counts (make-hash-table :test 'equal))
          (origin "git@gitolite.example:repo")
          requests browsed pushed fetched)
      (dolist (args `(("init" "--quiet" "-b" "topic")
                      ("remote" "add" "origin" ,origin)
                      ("remote" "add" "forgejo" ,remote-url)))
        (should (zerop (apply #'process-file "git" nil nil nil args))))
      (forgejo-test-with-temp-db
        (with-temp-buffer
          (cl-letf (((symbol-function 'keymap-popup) #'ignore)
                    ((symbol-function 'forgejo-api-get)
                     (lambda (&rest args) (push args requests)))
                    ((symbol-function 'browse-url)
                     (lambda (url &rest _) (push url browsed))))
            ;; An uncached menu first requests metadata.  Deliver its reply
            ;; after the command returns, then reopen to request both counts.
            (call-interactively #'forgejo-vc)
            (should (= (length requests) 1))
            (should (equal (seq-take (car requests) 3)
                           (list host-url "repos/owner/repo" nil)))
            (funcall (nth 3 (car requests))
                     '((name . "repo") (owner . ((login . "owner")))
                       (has_issues . t) (has_pull_requests . t)
                       (default_branch . "main")) nil)
            (setq requests nil)
            (call-interactively #'forgejo-vc)
            (should (= (length requests) 2))
            (should (equal (sort (mapcar
                                 (lambda (request)
                                   (should (equal (seq-take request 2)
                                                  (list host-url
                                                        "repos/owner/repo/issues")))
                                   (should (equal (assoc "state" (nth 2 request))
                                                  '("state" . "open")))
                                   (should (equal (assoc "limit" (nth 2 request))
                                                  '("limit" . "1")))
                                   (cdr (assoc "type" (nth 2 request))))
                                 requests) #'string<)
                           '("issues" "pulls")))
            (dolist (request requests)
              (funcall (nth 3 request) nil
                       (list :total-count
                             (if (equal (cdr (assoc "type" (nth 2 request)))
                                        "issues") 3 5))))
            (should (= (forgejo-vc--issue-count) 3))
            (should (= (forgejo-vc--pr-count) 5))
            (call-interactively #'forgejo-vc-browse)
            (should (equal browsed (list (concat host-url "/owner/repo"))))
            (should (equal (forgejo-vc--repo-from-remote)
                           (list host-url "owner" "repo" "forgejo"))))
          ;; Intercept Git launch, not remote discovery or argument building.
          (cl-letf (((symbol-function 'start-process)
                     (lambda (_name _buffer &rest command)
                       (setq pushed command)))
                    ((symbol-function 'set-process-sentinel) #'ignore)
                    ((symbol-function 'vc-git-branches) (lambda () '("topic")))
                    ((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq fetched (plist-get args :command))
                       (kill-buffer (plist-get args :stderr)))))
            (unwind-protect
                (progn
                  (forgejo-vc-submit "forgejo" "topic" "main" t)
                  (should (equal pushed '("git" "push" "-v" "forgejo"
                                          "HEAD:refs/for/main/topic"
                                          "-o" "force-push=true")))
                  (forgejo-vc-fetch 7)
                  (should (equal fetched '("git" "fetch" "forgejo"
                                           "pull/7/head"))))
              (when-let* ((buf (get-buffer "*forgejo PR*")))
                (kill-buffer buf))))))
      (should (equal (forgejo-vc--remotes) '("forgejo" "origin")))
      (should (equal (forgejo-vc--remote-url "origin") origin))
      (should (equal (forgejo-vc--remote-url "forgejo") remote-url)))))

(ert-deftest forgejo-test-vc-configured-http-web-port ()
  "HTTP web ports are independent of explicit SSH transport ports."
  (forgejo-test-vc--configured-endpoint
   "http://forge.example:3000" "ssh://git@forge.example:2222/owner/repo.git"))

(ert-deftest forgejo-test-vc-configured-https ()
  "Configured HTTPS is used for SCP-style SSH remotes."
  (forgejo-test-vc--configured-endpoint
   "https://forge.example" "git@forge.example:owner/repo.git"))

(ert-deftest forgejo-test-vc-configured-https-web-port ()
  "Configured HTTPS web ports do not inherit the SSH port."
  (forgejo-test-vc--configured-endpoint
   "https://forge.example:8443" "ssh://git@forge.example:2222/owner/repo.git"))

(provide 'forgejo-test-vc)
;;; forgejo-test-vc.el ends here
