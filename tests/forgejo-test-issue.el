;;; forgejo-test-issue.el --- Tests for forgejo-issue  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for issue-specific logic: entry formatting and API param building.

;;; Code:

(require 'forgejo-test-helper)
(require 'forgejo-issue)
(require 'forgejo-pull)

;;; Group 1: Entry conversion

(ert-deftest forgejo-test-issue-entries ()
  "Convert API issues to tabulated-list entries."
  (let* ((issues `(((number . 42)
                    (state . "open")
                    (title . "Test issue")
                    (labels . (((name . "bug") (color . "ff0000"))))
                    (user . ((login . "alice")))
                    (updated_at . "2020-06-15T10:00:00Z"))))
         (entries (forgejo-filter-list-entries issues)))
    (should (= (length entries) 1))
    (let ((entry (car entries)))
      (should (= (car entry) 42))
      (let ((cols (cadr entry)))
        (should (string= (aref cols 0) "42"))
        (should (string= (aref cols 2) "Test issue"))
        (should (string-match-p "alice" (aref cols 4)))))))

;;; Group 2: Build params

(ert-deftest forgejo-test-issue-build-params-default ()
  "Default params include type=issues and sort."
  (let ((forgejo-default-sort "recentupdate")
        (forgejo--api-default-limit 30))
    (let ((params (forgejo-issue--build-params nil)))
      (should (assoc "type" params))
      (should (string= (cdr (assoc "type" params)) "issues"))
      (should (assoc "sort" params)))))

(ert-deftest forgejo-test-issue-build-params-with-filters ()
  "Filters are included in params."
  (let ((forgejo-default-sort "recentupdate")
        (forgejo--api-default-limit 30))
    (let ((params (forgejo-issue--build-params
                   '(:state "open" :query "bug" :page 2))))
      (should (string= (cdr (assoc "state" params)) "open"))
      (should (string= (cdr (assoc "q" params)) "bug"))
      (should (string= (cdr (assoc "page" params)) "2")))))

;;; Group 3: Sync finalization

(ert-deftest forgejo-test-issue-sync-filtered-force-does-not-close-missing ()
  "Filtered forced syncs must not mark unrelated cached issues closed."
  (let (close-called sync-called)
    (cl-letf (((symbol-function 'forgejo-api-get) (lambda (&rest _args) nil))
              ((symbol-function 'forgejo-api-get-paged)
               (lambda (_host _endpoint _params _page-callback done-callback)
                 (funcall done-callback '(((number . 1))) '(:total-count 1))))
              ((symbol-function 'forgejo-db-close-missing)
               (lambda (&rest _args) (setq close-called t)))
              ((symbol-function 'forgejo-db-set-sync-time)
               (lambda (_host _owner _repo key _time) (setq sync-called key))))
      (forgejo-issue--sync "https://codeberg.org" "codeberg.org"
                           "owner" "repo" '(:state "open" :labels "bug")
                           " *forgejo-test-missing*" t)
      (should-not close-called)
      (should (equal sync-called
                     (forgejo-filter-sync-key "issues"
                                              '(:state "open" :labels "bug")))))))

(ert-deftest forgejo-test-issue-sync-partial-does-not-finalize ()
  "Partial forced syncs must not close missing issues or advance sync time."
  (let (close-called sync-called)
    (cl-letf (((symbol-function 'forgejo-api-get) (lambda (&rest _args) nil))
              ((symbol-function 'forgejo-api-get-paged)
               (lambda (_host _endpoint _params _page-callback done-callback)
                 (funcall done-callback '(((number . 1))) '(:partial t))))
              ((symbol-function 'forgejo-db-close-missing)
               (lambda (&rest _args) (setq close-called t)))
              ((symbol-function 'forgejo-db-set-sync-time)
               (lambda (_host _owner _repo key _time) (setq sync-called key))))
      (forgejo-issue--sync "https://codeberg.org" "codeberg.org"
                           "owner" "repo" '(:state "open")
                           " *forgejo-test-missing*" t)
      (should-not close-called)
      (should-not sync-called))))

;;; Group 4: Detail view entry

(ert-deftest forgejo-test-issue-view-passes-missing-comment-id-to-sync ()
  (let ((forgejo-repo--host "https://codeberg.org")
        sync-args)
    (unwind-protect
        (cl-letf (((symbol-function 'forgejo-db-get-issue)
                   (lambda (&rest _args) '((number . 1))))
                  ((symbol-function 'forgejo-db-get-timeline)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'forgejo-issue--render-detail)
                   (lambda (buf-name &rest _args) (get-buffer-create buf-name)))
                  ((symbol-function 'forgejo-issue--sync-detail)
                   (lambda (&rest args) (setq sync-args args))))
          (forgejo-issue-view "owner" "repo" 1 99)
          (should (equal sync-args
                         '("codeberg.org" "owner" "repo" 1
                           "*forgejo-issue: owner/repo#1*" nil 99))))
      (when-let* ((buf (get-buffer "*forgejo-issue: owner/repo#1*")))
        (kill-buffer buf)))))

(ert-deftest forgejo-test-issue-view-omits-present-comment-id-from-sync ()
  (let ((forgejo-repo--host "https://codeberg.org")
        sync-args)
    (unwind-protect
        (cl-letf (((symbol-function 'forgejo-db-get-issue)
                   (lambda (&rest _args) '((number . 1))))
                  ((symbol-function 'forgejo-db-get-timeline)
                   (lambda (&rest _args) nil))
                  ((symbol-function 'forgejo-issue--render-detail)
                   (lambda (buf-name &rest _args)
                     (with-current-buffer (get-buffer-create buf-name)
                       (setq forgejo-view--ewoc (ewoc-create #'ignore nil nil t))
                       (ewoc-enter-last forgejo-view--ewoc '(:type comment :id 99))
                       (current-buffer))))
                  ((symbol-function 'forgejo-issue--sync-detail)
                   (lambda (&rest args) (setq sync-args args))))
          (forgejo-issue-view "owner" "repo" 1 99)
          (should (equal sync-args
                         '("codeberg.org" "owner" "repo" 1
                           "*forgejo-issue: owner/repo#1*" nil nil))))
      (when-let* ((buf (get-buffer "*forgejo-issue: owner/repo#1*")))
        (kill-buffer buf)))))

(ert-deftest forgejo-test-issue-view-jumps-existing-buffer-window ()
  (let ((forgejo-repo--host "https://codeberg.org")
        (buf-name "*forgejo-issue: owner/repo#1*")
        (other (get-buffer-create "*forgejo-test-issue-other*")))
    (unwind-protect
        (cl-letf (((symbol-function 'forgejo-db-get-issue)
                   (lambda (&rest _args) (forgejo-test-detail-issue)))
                  ((symbol-function 'forgejo-db-get-timeline)
                   (lambda (&rest _args) (forgejo-test-timeline 10 20)))
                  ((symbol-function 'forgejo-db--row-to-timeline-alist)
                   #'identity)
                  ((symbol-function 'forgejo-buffer--update-reactions)
                   #'ignore)
                  ((symbol-function 'forgejo-issue--sync-detail)
                   #'ignore))
          (forgejo-issue-view "owner" "repo" 1)
          (forgejo-view--goto-comment forgejo-view--ewoc 10)
          (switch-to-buffer other)
          (forgejo-issue-view "owner" "repo" 1 20)
          (should (eql 20 (plist-get (forgejo-view--node-at-point) :id))))
      (when-let* ((buf (get-buffer buf-name)))
        (kill-buffer buf))
      (when (buffer-live-p other)
        (kill-buffer other)))))

;;; Cache synchronization journeys

(ert-deftest forgejo-test-issue-and-pull-filtered-reopen ()
  "A narrow fetch cannot hide old unseen or changed items on broad reopen."
  (dolist (type '(issue pull))
    (dolist (previous-sync '(nil "2026-01-01T00:00:00Z"))
      (forgejo-test-with-temp-db
        (let* ((sync (intern (format "forgejo-%s--sync" type)))
               (open-list (intern (format "forgejo-%s-list" type)))
               (kind (if (eq type 'pull) "pulls" "issues"))
               (buf-name (format "*forgejo-%s: owner/repo*" kind))
               (forgejo-hosts '(("https://forgejo.invalid")))
               (forgejo-issue-default-filter '("state:open"))
               (forgejo-pull-default-filter '("state:open"))
               (items (mapcar
                       (lambda (number)
                         (forgejo-test-issue
                          `((id . ,number) (number . ,number)
                            (title . ,(format "Item %s" number))
                            (updated_at . "2026-02-01T00:00:00Z")
                            (labels . ,(and (= number 1) (list (forgejo-test-label))))
                            (pull_request . ,(and (eq type 'pull) '((merged . :false)))))))
                       '(1 2 3)))
               requests)
          ;; Item 2 is unseen; item 3 changed outside the narrow label query.
          (forgejo-db-save-issues
           "forgejo.invalid" "owner" "repo"
           (list (forgejo-test-alist-merge (nth 2 items) '((title . "Stale")))))
          ;; Legacy shared cursors may already contain narrow-query coverage.
          (forgejo-db-set-sync-time
           "forgejo.invalid" "owner" "repo" kind "2099-01-01T00:00:00Z")
          (when previous-sync
            (forgejo-db-set-sync-time
             "forgejo.invalid" "owner" "repo"
             (forgejo-filter-sync-key kind '(:state "open")) previous-sync))
          (unwind-protect
              (save-window-excursion
                (cl-letf (((symbol-function 'forgejo-api-get)
                           (lambda (_host endpoint params callback &rest _args)
                             (when (string-suffix-p "/issues" endpoint)
                               (push params requests)
                               (let* ((since (cdr (assoc "since" params)))
                                      (label (cdr (assoc "labels" params)))
                                      (data (cl-remove-if-not
                                             (lambda (item)
                                               (and (or (not label) (= (alist-get 'number item) 1))
                                                    (or (not since)
                                                        (string-lessp since (alist-get 'updated_at item)))))
                                             items)))
                                 (funcall callback data (list :total-count (length data))))))))
                  (funcall sync "https://forgejo.invalid" "forgejo.invalid"
                           "owner" "repo" '(:state "open" :labels "bug") buf-name t)
                  (should-not (forgejo-db-get-issue "forgejo.invalid" "owner" "repo" 2))
                  (should (equal (alist-get 'title (forgejo-db-get-issue
                                                    "forgejo.invalid" "owner" "repo" 3)) "Stale"))
                  ;; Exercise the public ordinary reopen, not a forced refresh.
                  (funcall open-list "owner" "repo")
                  (should (equal (cdr (assoc "since" (car requests))) previous-sync))
                  (should-not (assoc "labels" (car requests)))
                  (should (forgejo-db-get-issue "forgejo.invalid" "owner" "repo" 2))
                  (should (equal (alist-get 'title (forgejo-db-get-issue
                                                    "forgejo.invalid" "owner" "repo" 3)) "Item 3"))))
            (when-let* ((buf (get-buffer buf-name))) (kill-buffer buf))))))))

(ert-deftest forgejo-test-issue-and-pull-pagination-cache-safety ()
  "Capped pages preserve open items; incomplete pages cannot finalize."
  (dolist (type '(issue pull))
    (dolist (incomplete '(nil t))
      (forgejo-test-with-temp-db
        (let* ((sync (intern (format "forgejo-%s--sync" type)))
               (kind (if (eq type 'pull) "pulls" "issues"))
               (key (forgejo-filter-sync-key kind '(:state "open")))
               (old-time "2026-01-01T00:00:00Z")
               (items (mapcar
                       (lambda (number)
                         (forgejo-test-issue
                          `((id . ,number) (number . ,number)
                            (pull_request . ,(and (eq type 'pull) '((merged . :false)))))))
                       '(1 2 3)))
               requests)
          (forgejo-db-save-issues "forgejo.invalid" "owner" "repo" items)
          (forgejo-db-set-sync-time "forgejo.invalid" "owner" "repo" key old-time)
          (cl-letf (((symbol-function 'forgejo-api-get)
                     (lambda (_host endpoint params callback &rest _args)
                       (when (string-suffix-p "/issues" endpoint)
                         (let ((page (string-to-number (cdr (assoc "page" params)))))
                           (push page requests)
                           (should (<= page 2))
                           (funcall callback
                                    (if (= page 1) (seq-take items 2)
                                      (unless incomplete (nthcdr 2 items)))
                                    (if (= page 1)
                                        '(:total-count 3 :link "<https://forgejo.invalid/items?page=2>; rel=\"next\"")
                                      '(:total-count 3))))))))
            (funcall sync "https://forgejo.invalid" "forgejo.invalid"
                     "owner" "repo" '(:state "open") " *forgejo-test-absent*" t))
          (should (equal (nreverse requests) '(1 2)))
          (should (equal (alist-get 'state (forgejo-db-get-issue
                                            "forgejo.invalid" "owner" "repo" 3)) "open"))
          (should (eq incomplete
                      (equal old-time (forgejo-db-get-sync-time
                                       "forgejo.invalid" "owner" "repo" key)))))))))

(ert-deftest forgejo-test-issue-and-pull-sync-cursor-start-time ()
  "A delayed completion records the request start, not the callback time."
  (dolist (type '(issue pull))
    (let ((clock "2026-01-01T00:00:00Z") done saved)
      (cl-letf (((symbol-function 'format-time-string) (lambda (&rest _) clock))
                ((symbol-function 'forgejo-api-get) #'ignore)
                ((symbol-function 'forgejo-api-get-paged)
                 (lambda (_host _endpoint _params _page-callback callback)
                   (setq done callback)))
                ((symbol-function 'forgejo-db-set-sync-time)
                 (lambda (_host _owner _repo _key time) (setq saved time))))
        (funcall (intern (format "forgejo-%s--sync" type))
                 "https://forgejo.invalid" "forgejo.invalid"
                 "owner" "repo" '(:state "closed") " *forgejo-test-absent*" t)
        (setq clock "2026-01-02T00:00:00Z")
        (funcall done nil '(:total-count 0))
        (should (equal saved "2026-01-01T00:00:00Z"))))))

(provide 'forgejo-test-issue)
;;; forgejo-test-issue.el ends here
