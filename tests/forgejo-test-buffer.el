;;; forgejo-test-buffer.el --- Tests for forgejo-buffer  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for shared display utilities: state formatting, label
;; colorization, relative time, login extraction, and EWOC node building.

;;; Code:

(require 'forgejo-test-helper)
(require 'forgejo-buffer)
(require 'forgejo-review)
(require 'forgejo-view)
(require 'forgejo-pull)

;;; Group 1: State formatting

(ert-deftest forgejo-test-buffer-format-state-open ()
  "Open state uses the open face."
  (let ((result (forgejo-buffer--format-state "open")))
    (should (string= result "open"))
    (should (eq (get-text-property 0 'face result) 'forgejo-open-face))))

(ert-deftest forgejo-test-buffer-format-state-closed ()
  "Closed state uses the closed face."
  (let ((result (forgejo-buffer--format-state "closed")))
    (should (eq (get-text-property 0 'face result) 'forgejo-closed-face))))

;;; Group 2: Label formatting

(ert-deftest forgejo-test-buffer-format-labels ()
  "Labels are joined with commas and propertized with readable colors."
  (let* ((labels '(((name . "bug") (color . "d73a4a"))
                   ((name . "help") (color . "0075ca"))))
         (result (forgejo-buffer--format-labels labels)))
    (should (string-match-p "bug" result))
    (should (string-match-p "help" result))
    (should (string-match-p ", " result))
    (should (eq (plist-get (get-text-property 0 'face result) :weight) 'bold))
    (should (plist-get (get-text-property 0 'face result) :foreground))))

(ert-deftest forgejo-test-buffer-format-labels-empty ()
  "Empty labels return empty string."
  (should (string= (forgejo-buffer--format-labels nil) ""))
  (should (string= (forgejo-buffer--format-labels '()) "")))

;;; Group 3: Relative time

(ert-deftest forgejo-test-buffer-relative-time-nil ()
  "Nil or empty time returns empty string."
  (should (string= (forgejo-buffer--relative-time nil) ""))
  (should (string= (forgejo-buffer--relative-time "") "")))

(ert-deftest forgejo-test-buffer-relative-time-old ()
  "Very old time returns a date string."
  (let ((result (forgejo-buffer--relative-time "2020-01-01T00:00:00Z")))
    (should (string-match-p "2020-01-01" result))))

;;; Group 4: Login extraction

(ert-deftest forgejo-test-buffer-login ()
  "Extract login from user alist."
  (should (string= (forgejo-buffer--login '((login . "alice"))) "alice"))
  (should (null (forgejo-buffer--login :null)))
  (should (null (forgejo-buffer--login nil))))

;;; Group 5: EWOC node building

(ert-deftest forgejo-test-buffer-build-nodes ()
  "Build EWOC nodes from issue data and timeline."
  (let* ((issue '((number . 42) (title . "Test") (state . "open")
                  (body . "Description") (user . ((login . "alice")))
                  (labels) (milestone) (comments . 2)
                  (created_at . "2026-01-01T00:00:00Z")))
         (timeline `(((type . "comment") (body . "LGTM")
                      (user . ((login . "bob")))
                      (created_at . "2026-01-02T00:00:00Z"))
                     ((type . "close") (user . ((login . "alice")))
                      (created_at . "2026-01-03T00:00:00Z"))))
         (nodes (forgejo-buffer--build-nodes issue timeline)))
    (should (= (length nodes) 3))
    (should (eq (plist-get (nth 0 nodes) :type) 'header))
    (should (eq (plist-get (nth 1 nodes) :type) 'comment))
    (should (eq (plist-get (nth 2 nodes) :type) 'event))))

(ert-deftest forgejo-test-buffer-build-nodes-empty-timeline ()
  "Issue with no timeline produces only header node."
  (let* ((issue '((number . 1) (title . "Solo") (state . "open")
                  (body . "") (user . ((login . "me")))
                  (labels) (milestone) (comments . 0)
                  (created_at . "2026-01-01T00:00:00Z")))
         (nodes (forgejo-buffer--build-nodes issue nil)))
    (should (= (length nodes) 1))
    (should (eq (plist-get (car nodes) :type) 'header))))

;;; Review decisions

(defconst forgejo-test-buffer--review-decisions
  '(("APPROVED" "approved" forgejo-review-approved-face)
    ("REQUEST_CHANGES" "requested changes" forgejo-review-rejected-face)
    ("COMMENT" "commented" forgejo-review-comment-face)
    ("FUTURE_STATE" "reviewed" shadow)
    (nil "reviewed" shadow))
  "Review states, displayed verbs and faces for timeline regressions.")

(defun forgejo-test-buffer--review-event (state)
  "Return a body-bearing timeline review with STATE."
  (forgejo-test-comment
   31 `((type . "review") (review_id . 3) (review_state . ,state)
        (body . "Review body: keep this text."))))

(ert-deftest forgejo-test-buffer-review-decision-nodes ()
  "Threadless review nodes retain decisions and comment action identity."
  (dolist (case forgejo-test-buffer--review-decisions)
    (ert-info ((format "Review state %S" (car case)))
      (let* ((event (forgejo-test-buffer--review-event (car case)))
             (nodes (forgejo-buffer--node-review event "commenter" (list event)))
             (node (car nodes)))
        (should (= (length nodes) 1))
        (should (eq (plist-get node :type) 'comment))
        (should (plist-member node :review-state))
        (should (equal (plist-get node :review-state) (car case)))
        (should (= (plist-get node :id) 31))
        (should (equal (plist-get node :author) "commenter"))
        (should (equal (plist-get node :body) (alist-get 'body event)))
        (should (equal (plist-get node :created-at)
                       (alist-get 'created_at event)))))))

(ert-deftest forgejo-test-buffer-review-decision-rendered ()
  "Rendered reviews show the decision, body, author, time and comment ID."
  (dolist (case forgejo-test-buffer--review-decisions)
    (ert-info ((format "Review state %S" (car case)))
      (with-temp-buffer
        (forgejo-pull-view-mode)
        (let* ((event (forgejo-test-buffer--review-event (car case)))
               (nodes (forgejo-buffer--build-nodes
                       (forgejo-test-detail-pr) (list event))))
          (forgejo-buffer--fontify-node-bodies nodes)
          (forgejo-view--populate-ewoc nodes)
          (goto-char (ewoc-location (ewoc-nth forgejo-view--ewoc 1)))
          (should (looking-at
                   (regexp-quote (concat "commenter " (nth 1 case) " "))))
          (should (eq (get-text-property (point) 'face)
                      'forgejo-comment-author-face))
          (should (eq (get-text-property (+ (point) (length "commenter "))
                                         'face)
                      (nth 2 case)))
          (should (search-forward
                   (forgejo-buffer--relative-time (alist-get 'created_at event))
                   (line-end-position) t))
          (should (eq (get-text-property (1- (point)) 'face) 'shadow))
          (should (search-forward (alist-get 'body event) nil t))
          (should (= (forgejo-view--comment-id-at-point) 31)))))))

(ert-deftest forgejo-test-buffer-review-decision-ordinary-comment ()
  "Ordinary comments keep their node, edited marker, text and actions."
  (let* ((event (forgejo-test-comment
                 32 '((updated_at . "2026-01-02T00:00:00Z"))))
         (node (forgejo-buffer--node-comment event "commenter")))
    (should-not (plist-member node :review-state))
    (should (equal node
                   '(:type comment :id 32 :author "commenter" :body "Comment 32"
                     :created-at "2026-01-01T00:00:00Z"
                     :updated-at "2026-01-02T00:00:00Z")))
    (with-temp-buffer
      (forgejo-pull-view-mode)
      (forgejo-view--populate-ewoc (list node))
      (goto-char (point-min))
      (should (looking-at "commenter commented "))
      (should (eq (get-text-property (+ (point) (length "commenter ")) 'face)
                  'shadow))
      (should (search-forward "(edited)" (line-end-position) t))
      (should (search-forward "Comment 32" nil t))
      (should (= (forgejo-view--comment-id-at-point) 32)))))

(ert-deftest forgejo-test-buffer-review-decision-threaded ()
  "Threaded reviews retain decision rendering and actionable thread links."
  (dolist (case forgejo-test-buffer--review-decisions)
    (ert-info ((format "Review state %S" (car case)))
      (let* ((event (forgejo-test-buffer--review-event (car case)))
             (timeline (list event
                             '((id . 33) (type . "review_comment")
                               (review_id . 3) (path . "lisp/example.el")
                               (position . 2) (original_position . 1)
                               (body . "Inline comment"))))
             (nodes (forgejo-buffer--build-nodes
                     (forgejo-test-detail-pr) timeline))
             (node (cadr nodes)))
        (should (= (length nodes) 2))
        (should (eq (plist-get node :type) 'review-link))
        (should (equal (plist-get node :review-state) (car case)))
        (should (equal (plist-get node :body) (alist-get 'body event)))
        (should (equal (plist-get node :threads)
                       '((:count 1 :path "lisp/example.el" :position 2
                          :original-position 1 :diff-hunk nil :resolved nil))))
        (with-temp-buffer
          (forgejo-pull-view-mode)
          (forgejo-buffer--fontify-node-bodies nodes)
          (forgejo-view--populate-ewoc nodes)
          (goto-char (ewoc-location (ewoc-nth forgejo-view--ewoc 1)))
          (should (looking-at
                   (regexp-quote (concat "commenter " (nth 1 case) " "))))
          (should (eq (get-text-property (+ (point) (length "commenter "))
                                         'face)
                      (nth 2 case)))
          (should (search-forward "[1 comment on example.el]" nil t))
          (let ((pos (1- (point))))
            (should (= (get-text-property pos 'forgejo-review-id) 3))
            (should (equal (get-text-property pos 'forgejo-review-path)
                           "lisp/example.el"))
            (should (= (get-text-property pos 'forgejo-review-position) 2))
            (should (= (get-text-property pos 'forgejo-review-opos) 1))
            (should (eq (get-text-property pos 'keymap)
                        forgejo-buffer--action-map)))
          (should (search-forward "(unresolved)" nil t))
          (should (search-forward (alist-get 'body event) nil t))
          (should-not (forgejo-view--comment-id-at-point)))))))

;;; Group 6: Clean body

(ert-deftest forgejo-test-buffer-clean-body ()
  "Strip carriage returns, handle nil and :null."
  (should (string= (forgejo-buffer--clean-body "hello\r\nworld") "hello\nworld"))
  (should (null (forgejo-buffer--clean-body nil)))
  (should (null (forgejo-buffer--clean-body :null)))
  (should (null (forgejo-buffer--clean-body ""))))

;;; Group 7: Node keys

(ert-deftest forgejo-test-buffer-node-key-header ()
  (should (equal (forgejo-buffer--node-key '(:type header :number 1))
                 '(header))))

(ert-deftest forgejo-test-buffer-node-key-comment ()
  (should (equal (forgejo-buffer--node-key '(:type comment :id 42))
                 '(comment . 42))))

(ert-deftest forgejo-test-buffer-node-key-event ()
  (should (equal (forgejo-buffer--node-key
                  '(:type event :id 7 :event-type "closed"))
                 '(event . 7))))

(ert-deftest forgejo-test-buffer-node-key-review-link ()
  (should (equal (forgejo-buffer--node-key
                  '(:type review-link :review-id 99))
                 '(review-link . 99))))

;;; Group 8: Build-event-node stamps :id

(ert-deftest forgejo-test-buffer-build-event-node-stamps-id ()
  "Every non-header event node carries :id from the source event."
  (let* ((events '(((id . 11) (type . "close") (user . ((login . "a")))
                    (created_at . "2026-01-01T00:00:00Z"))
                   ((id . 12) (type . "label") (body . "1")
                    (label . ((name . "bug") (color . "ff0000")))
                    (user . ((login . "a")))
                    (created_at . "2026-01-02T00:00:00Z"))))
         (nodes (mapcar (lambda (e)
                          (forgejo-buffer--build-event-node e "a" events))
                        events)))
    (should (= 11 (plist-get (nth 0 nodes) :id)))
    (should (= 12 (plist-get (nth 1 nodes) :id)))))

;;; Group 9: Reactions

(ert-deftest forgejo-test-buffer-reaction-label-emoji-path ()
  "Known reactions use emoji labels when displayable."
  (cl-letf (((symbol-function 'forgejo-buffer--displayable-reaction-emoji-p)
             (lambda (_emoji) t)))
    (should (string= (forgejo-buffer--reaction-label "heart") "❤️"))))

(ert-deftest forgejo-test-buffer-reaction-label-fallback-path ()
  "Known reactions keep text labels when emoji is not displayable."
  (cl-letf (((symbol-function 'forgejo-buffer--displayable-reaction-emoji-p)
             (lambda (_emoji) nil)))
    (should (string= (forgejo-buffer--reaction-label "rocket") "rocket"))))

(ert-deftest forgejo-test-buffer-reaction-label-unknown ()
  "Unknown reactions keep their original content."
  (cl-letf (((symbol-function 'forgejo-buffer--displayable-reaction-emoji-p)
             (lambda (_emoji) nil)))
    (should (string= (forgejo-buffer--reaction-label "custom") "custom"))))

(ert-deftest forgejo-test-buffer-reaction-help-echo-prefixes-label ()
  "Help-echo names the reaction label and each user that reacted."
  (cl-letf (((symbol-function 'forgejo-buffer--displayable-reaction-emoji-p)
             (lambda (_emoji) t)))
    (should (equal (forgejo-buffer--reaction-help-echo "heart" '("alice" "bob"))
                   "❤️: alice, bob"))))

(ert-deftest forgejo-test-buffer-reaction-help-echo-nil-without-users ()
  "Help-echo is nil when no users reacted with the content."
  (should-not (forgejo-buffer--reaction-help-echo "heart" nil)))

;;; Group 10: Reconcile

(defun forgejo-test-buffer--make-ewoc (nodes)
  "Build a fresh EWOC populated with NODES for testing reconcile."
  (with-current-buffer (generate-new-buffer " *fbtest*")
    (let ((ewoc (ewoc-create (lambda (data)
                               (insert (format "%S\n" data)))
                             nil nil t)))
      (dolist (n nodes)
        (ewoc-enter-last ewoc n))
      ewoc)))

(defun forgejo-test-buffer--ewoc-keys (ewoc)
  "Return the list of node keys currently in EWOC, in order."
  (let (keys (n (ewoc-nth ewoc 0)))
    (while n
      (push (forgejo-buffer--node-key (ewoc-data n)) keys)
      (setq n (ewoc-next ewoc n)))
    (nreverse keys)))

(ert-deftest forgejo-test-buffer-reconcile-noop ()
  "Reconciling identical node lists makes no changes."
  (let* ((nodes '((:type header :number 1 :title "t")
                  (:type comment :id 10 :body "a")
                  (:type event :id 11 :event-type "closed")))
         (ewoc (forgejo-test-buffer--make-ewoc nodes))
         (invalidated 0))
    (unwind-protect
        (cl-letf* ((orig (symbol-function 'ewoc-invalidate))
                   ((symbol-function 'ewoc-invalidate)
                    (lambda (&rest args) (cl-incf invalidated)
                      (apply orig args))))
          (forgejo-buffer--reconcile-ewoc ewoc (copy-tree nodes))
          (should (= invalidated 0))
          (should (equal (forgejo-test-buffer--ewoc-keys ewoc)
                         '((header) (comment . 10) (event . 11)))))
      (kill-buffer (ewoc-buffer ewoc)))))

(ert-deftest forgejo-test-buffer-reconcile-insert ()
  "Reconciling with a new comment inserts it at the right position."
  (let* ((old '((:type header :number 1)
                (:type comment :id 10 :body "first")
                (:type event :id 11 :event-type "closed")))
         (new '((:type header :number 1)
                (:type comment :id 10 :body "first")
                (:type comment :id 12 :body "new")
                (:type event :id 11 :event-type "closed")))
         (ewoc (forgejo-test-buffer--make-ewoc old)))
    (unwind-protect
        (progn
          (forgejo-buffer--reconcile-ewoc ewoc new)
          (should (equal (forgejo-test-buffer--ewoc-keys ewoc)
                         '((header) (comment . 10) (comment . 12)
                           (event . 11)))))
      (kill-buffer (ewoc-buffer ewoc)))))

(ert-deftest forgejo-test-buffer-reconcile-update ()
  "Reconciling a comment with changed body updates and invalidates it."
  (let* ((old '((:type comment :id 10 :body "old")))
         (new '((:type comment :id 10 :body "new")))
         (ewoc (forgejo-test-buffer--make-ewoc old))
         (invalidated 0))
    (unwind-protect
        (cl-letf* ((orig (symbol-function 'ewoc-invalidate))
                   ((symbol-function 'ewoc-invalidate)
                    (lambda (&rest args) (cl-incf invalidated)
                      (apply orig args))))
          (forgejo-buffer--reconcile-ewoc ewoc new)
          (should (= invalidated 1))
          (should (equal (plist-get (ewoc-data (ewoc-nth ewoc 0)) :body)
                         "new")))
      (kill-buffer (ewoc-buffer ewoc)))))

(ert-deftest forgejo-test-buffer-reconcile-delete ()
  "Reconciling with a node removed deletes it from the EWOC."
  (let* ((old '((:type comment :id 10 :body "a")
                (:type comment :id 11 :body "b")))
         (new '((:type comment :id 10 :body "a")))
         (ewoc (forgejo-test-buffer--make-ewoc old)))
    (unwind-protect
        (progn
          (forgejo-buffer--reconcile-ewoc ewoc new)
          (should (equal (forgejo-test-buffer--ewoc-keys ewoc)
                         '((comment . 10)))))
      (kill-buffer (ewoc-buffer ewoc)))))

(ert-deftest forgejo-test-buffer-reconcile-preserves-reactions ()
  "Reactions previously patched onto a node survive reconcile when the
new node doesn't carry :reactions."
  (let* ((old '((:type comment :id 10 :body "a"
                       :reactions (("heart" "alice")))))
         (new '((:type comment :id 10 :body "a")))
         (ewoc (forgejo-test-buffer--make-ewoc old)))
    (unwind-protect
        (progn
          (forgejo-buffer--reconcile-ewoc ewoc new)
          (should (equal (plist-get (ewoc-data (ewoc-nth ewoc 0)) :reactions)
                         '(("heart" "alice")))))
      (kill-buffer (ewoc-buffer ewoc)))))

;;; Group: Reference linkification

(defun forgejo-test-buffer--linkify (text)
  "Insert TEXT into a temp buffer and run `forgejo-buffer--linkify-refs'.
Return a list of (BEG END NUMBER REPO) for each ref found."
  (with-temp-buffer
    (insert text)
    (forgejo-buffer--linkify-refs (point-min) (point-max))
    (let ((pos (point-min)) refs)
      (while (< pos (point-max))
        (if-let* ((n (get-text-property pos 'forgejo-ref-number)))
            (let ((end (or (next-single-property-change
                            pos 'forgejo-ref-number)
                           (point-max))))
              (push (list pos end n
                          (get-text-property pos 'forgejo-ref-repo))
                    refs)
              (setq pos end))
          (setq pos (or (next-single-property-change
                         pos 'forgejo-ref-number)
                        (point-max)))))
      (nreverse refs))))

(ert-deftest forgejo-test-buffer-linkify-bare-hash ()
  "Bare #N is linkified."
  (let ((refs (forgejo-test-buffer--linkify "see #42 for context")))
    (should (= (length refs) 1))
    (should (= (nth 2 (car refs)) 42))
    (should (null (nth 3 (car refs))))))

(ert-deftest forgejo-test-buffer-linkify-bare-bang ()
  "Bare !N is linkified (Forgejo PR shorthand)."
  (let ((refs (forgejo-test-buffer--linkify "merged in !17 yesterday")))
    (should (= (length refs) 1))
    (should (= (nth 2 (car refs)) 17))
    (should (null (nth 3 (car refs))))))

(ert-deftest forgejo-test-buffer-linkify-qualified-hash ()
  "owner/repo#N is linkified with repo captured."
  (let ((refs (forgejo-test-buffer--linkify "guix/guix#8544 fixes it")))
    (should (= (length refs) 1))
    (should (= (nth 2 (car refs)) 8544))
    (should (string= (nth 3 (car refs)) "guix/guix"))))

(ert-deftest forgejo-test-buffer-linkify-qualified-bang ()
  "owner/repo!N is linkified with repo captured."
  (let ((refs (forgejo-test-buffer--linkify "see guix/guix!8641 for the PR")))
    (should (= (length refs) 1))
    (should (= (nth 2 (car refs)) 8641))
    (should (string= (nth 3 (car refs)) "guix/guix"))))

(ert-deftest forgejo-test-buffer-linkify-no-match-plain-text ()
  "Plain text without a #N or !N produces no refs."
  (should (null (forgejo-test-buffer--linkify "no references here"))))

(ert-deftest forgejo-test-buffer-linkify-multiple ()
  "Multiple refs in one body are all linkified."
  (let ((refs (forgejo-test-buffer--linkify "see #1 and guix/guix!2 and #3")))
    (should (= (length refs) 3))
    (should (equal (mapcar (lambda (r) (nth 2 r)) refs) '(1 2 3)))))

(provide 'forgejo-test-buffer)
;;; forgejo-test-buffer.el ends here
