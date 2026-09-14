;;; forgejo-test-dev.el --- Development tooling tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Run in disposable Emacs only: these tests reload package sources.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'forgejo-dev)
(require 'forgejo)
(require 'forgejo-utils)

(defconst forgejo-test-dev--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defun forgejo-test-dev--sources ()
  "Return the Makefile's package source list."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "Makefile" forgejo-test-dev--root))
    (goto-char (point-min))
    (re-search-forward "^SRCS = ")
    (let ((start (point)))
      (re-search-forward "\n\n")
      (split-string (replace-regexp-in-string
                     "\\\\\n" " " (buffer-substring start (match-beginning 0)))))))

(ert-deftest forgejo-test-dev-repeated-load-retains-maps-and-dispatch ()
  (let ((files (forgejo-test-dev--sources)))
    (forgejo-dev-load forgejo-test-dev--root files)
    (let* ((maps (mapcar (lambda (symbol) (cons symbol (symbol-value symbol)))
                         forgejo-dev--maps))
           (forgejo-hosts '(("https://example.invalid")))
           (buffers (mapcar (lambda (entry)
                              (let ((buffer (generate-new-buffer " *forgejo-dev*")))
                                (with-current-buffer buffer (use-local-map (cdr entry)))
                                buffer))
                            maps))
           (unrelated (make-sparse-keymap)))
      (unwind-protect
          (progn
            (define-key unrelated "z" #'ignore)
            ;; No Customize metadata: plain key edits must survive.
            (define-key (alist-get 'forgejo-watch-list-mode-map maps) "z" #'ignore)
            (dotimes (_ 2)
              (forgejo-dev-load forgejo-test-dev--root files)
              (should (equal forgejo-hosts '(("https://example.invalid"))))
              (cl-mapc (lambda (buffer entry)
                         (with-current-buffer buffer
                           (should (eq (current-local-map) (symbol-value (car entry))))))
                       buffers maps))
            (should (eq (lookup-key unrelated "z") #'ignore))
            (should (eq (lookup-key (symbol-value 'forgejo-watch-list-mode-map) "z") #'ignore))
            (let (called)
              (cl-letf (((symbol-function 'forgejo-repo-search)
                         (lambda () (interactive) (setq called t))))
                (with-temp-buffer
                  (use-local-map (symbol-value 'forgejo-map))
                  (call-interactively (key-binding "s"))))
              (should called)))
        (mapc #'kill-buffer buffers)))))

(ert-deftest forgejo-test-dev-failure-restores-maps-and-stops ()
  (let ((files (forgejo-test-dev--sources)))
    (forgejo-dev-load forgejo-test-dev--root files)
    (dolist (failure '(error quit))
      (dolist (position (list 0 (1- (length files))))
        (let* ((maps (mapcar (lambda (symbol) (cons symbol (symbol-value symbol)))
                            forgejo-dev--maps))
               (contents (mapcar (lambda (entry) (copy-tree (cdr entry))) maps))
               (loader (symbol-function 'load))
               (count 0)
               caught)
          (with-temp-buffer
            (use-local-map (alist-get 'forgejo-watch-list-mode-map maps))
            (let ((retained (current-local-map)))
              (condition-case condition
                  (cl-letf (((symbol-function 'load)
                             (lambda (file &rest args)
                               (if (member file (mapcar (lambda (source)
                                                        (expand-file-name source forgejo-test-dev--root))
                                                      files))
                                   (prog1 (apply loader file args)
                                     (when (= (prog1 count (cl-incf count)) position)
                                       (signal failure '("Injected reload failure"))))
                                 (apply loader file args)))))
                    (forgejo-dev-load forgejo-test-dev--root files))
                ((error quit) (setq caught (car condition))))
              (should (eq caught failure))
              (should (= count (1+ position)))
              (should (eq (current-local-map) retained))))
          (cl-mapc (lambda (entry content)
                     (should (eq (symbol-value (car entry)) (cdr entry)))
                     (should (equal (cdr entry) content)))
                   maps contents))))))

(ert-deftest forgejo-test-dev-preserves-bindings-against-defaults ()
  (let ((old (make-sparse-keymap)) (new (make-sparse-keymap)))
    (define-key old "a" #'forward-char)
    (define-key old "b" #'backward-char)
    (let ((defaults (copy-keymap old)))
      (define-key old "b" #'ignore)
      (define-key new "a" #'next-line)
      (define-key new "b" #'previous-line)
      (forgejo-dev--preserve-bindings old new defaults)
      (should (eq (lookup-key new "a") #'next-line))
      (should (eq (lookup-key new "b") #'ignore)))))

(ert-deftest forgejo-test-dev-checkdoc-rejects-diagnostic ()
  (let* ((directory (make-temp-file "forgejo-checkdoc" t))
         (file (expand-file-name "forgejo-bad-doc.el" directory))
         (default-directory forgejo-test-dev--root))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";;; forgejo-bad-doc.el --- Fixture -*- lexical-binding: t; -*-\n"
                    ";;; Commentary:\n;; Fixture.\n;;; Code:\n"
                    "(defun forgejo-bad-doc (argument)\n  \"Return result.\"\n  argument)\n"
                    "(provide 'forgejo-bad-doc)\n;;; forgejo-bad-doc.el ends here\n"))
          (with-temp-buffer
            (should-not (zerop (call-process "make" nil t nil "lint"
                                            "FORGEJO_ENV_WRAPPED=1"
                                            (concat "SRCS=" file))))
            (should (string-match-p "ARGUMENT" (buffer-string)))))
      (delete-directory directory t))))

(ert-deftest forgejo-test-dev-map-publication-failure-rolls-back ()
  (let ((files (forgejo-test-dev--sources)))
    (forgejo-dev-load forgejo-test-dev--root files)
    (dolist (failure '(error quit))
      (let* ((maps (mapcar (lambda (symbol) (cons symbol (symbol-value symbol)))
                           forgejo-dev--maps))
             (contents (mapcar (lambda (entry) (copy-tree (cdr entry))) maps))
             (installer (symbol-function 'forgejo-dev--install-map))
             (count 0)
             caught)
        (condition-case condition
            (cl-letf (((symbol-function 'forgejo-dev--install-map)
                       (lambda (symbol old new)
                         (prog1 (funcall installer symbol old new)
                           (when (= (cl-incf count) 2)
                             (signal failure '("Injected map publication failure")))))))
              (forgejo-dev-load forgejo-test-dev--root files))
          ((error quit) (setq caught (car condition))))
        (should (eq caught failure))
        (cl-mapc (lambda (entry content)
                   (should (eq (symbol-value (car entry)) (cdr entry)))
                   (should (equal (cdr entry) content)))
                 maps contents)))))

(ert-deftest forgejo-test-dev-watch-and-search-mode-dispatch ()
  (let ((files (forgejo-test-dev--sources)))
    (forgejo-dev-load forgejo-test-dev--root files)
    (dolist (case '((forgejo-watch-list-mode forgejo-watch-view-at-point)
                    (forgejo-repo-search-mode forgejo-repo-action-at-point)))
      (with-temp-buffer
        (funcall (car case))
        (let ((map (current-local-map)) called)
          (forgejo-dev-load forgejo-test-dev--root files)
          (should (eq map (current-local-map)))
          (should (eq (key-binding (kbd "RET")) (cadr case)))
          (cl-letf (((symbol-function (cadr case))
                     (lambda () (interactive) (setq called t))))
            (call-interactively (key-binding (kbd "RET"))))
          (should called))))))

(ert-deftest forgejo-test-dev-nil-map-and-preflight ()
  (let ((files (forgejo-test-dev--sources)))
    (forgejo-dev-load forgejo-test-dev--root files)
    (let ((forgejo-map nil))
      (should-error (forgejo-dev-load forgejo-test-dev--root '("missing.el")))
      (should-not forgejo-map))
    (with-temp-buffer
      (setq-local forgejo-map (make-sparse-keymap))
      (should-error (forgejo-dev-load forgejo-test-dev--root files)))
    (let ((watcher (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            (add-variable-watcher 'forgejo-map watcher)
            (should-error (forgejo-dev-load forgejo-test-dev--root files)))
        (remove-variable-watcher 'forgejo-map watcher)))))

(ert-deftest forgejo-test-dev-client-override-and-single-eval ()
  (let* ((directory (make-temp-file "forgejo-client" t))
         (client (expand-file-name "client" directory))
         (calls (expand-file-name "calls" directory))
         (default-directory forgejo-test-dev--root))
    (unwind-protect
        (progn
          (with-temp-file client
            (insert "#!" (executable-find "sh") "\nprintf '%s\\n' \"$@\" >> \"" calls
                    "\"\nexit 19\n"))
          (set-file-modes client #o700)
          (with-temp-buffer
            (should-not (zerop (call-process "make" nil t nil "load"
                                            (concat "EMACSCLIENT=" client))))
            (should-not (string-match-p "Loaded all modules" (buffer-string))))
          (with-temp-buffer
            (insert-file-contents calls)
            (should (equal (buffer-substring (point-min) (line-end-position)) "--eval"))
            (forward-line)
            (let ((form (read (current-buffer))))
              (should (eq (car form) 'progn))
              (should (eq (car (nth 2 form)) 'forgejo-dev-load)))
            (skip-chars-forward "\n ")
            (should (eobp))))
      (delete-directory directory t))))

(ert-deftest forgejo-test-dev-preserves-vc-prefix-customization ()
  (let ((files (forgejo-test-dev--sources)))
    (forgejo-dev-load forgejo-test-dev--root files)
    (let* ((map (symbol-value 'vc-prefix-map))
           (original (lookup-key map "f")))
      (unwind-protect
          (dolist (binding '(ignore nil))
            (define-key map "f" binding)
            (forgejo-dev-load forgejo-test-dev--root files)
            (should (eq (lookup-key map "f") binding)))
        (define-key map "f" original)))))

(ert-deftest forgejo-test-dev-compose-repeated-load-dispatch ()
  (let ((files (forgejo-test-dev--sources)))
    (forgejo-dev-load forgejo-test-dev--root files)
    (let* ((forgejo-compose-mode-map (copy-keymap forgejo-compose-mode-map))
           (retained forgejo-compose-mode-map)
           (unrelated (cons 'forgejo-test-unrelated-mode (make-sparse-keymap)))
           (minor-mode-map-alist
            (cons unrelated (mapcar #'copy-sequence minor-mode-map-alist))))
      (setf (alist-get 'forgejo-compose-mode minor-mode-map-alist) retained)
      (define-key retained (kbd "C-c C-c") #'ignore)
      (with-temp-buffer
        (text-mode)
        (forgejo-compose-mode 1)
        (should (eq (key-binding (kbd "C-c C-c")) #'ignore))
        (dotimes (_ 2)
          (forgejo-dev-load forgejo-test-dev--root files)
          (should (eq forgejo-compose-mode-map retained))
          (should (eq (alist-get 'forgejo-compose-mode minor-mode-map-alist) retained))
          (should (eq (key-binding (kbd "C-c C-c")) #'ignore))
          (should (eq (assq 'forgejo-test-unrelated-mode minor-mode-map-alist) unrelated)))
        ;; A fresh top-level event detects stale maps sharing only prefix tails.
        (should-not (lookup-key forgejo-compose-mode-map (kbd "C-z")))
        (define-key forgejo-compose-mode-map (kbd "C-z") #'backward-char)
        (should (eq (key-binding (kbd "C-z")) #'backward-char))
        (insert "x")
        (call-interactively (key-binding (kbd "C-z")))
        (should (= (point) (point-min)))))))

(ert-deftest forgejo-test-dev-compose-failure-restores-dispatch ()
  (let ((files (forgejo-test-dev--sources)))
    (forgejo-dev-load forgejo-test-dev--root files)
    (dolist (failure '(error quit))
      ;; Fail after the final load, or before/after publishing the compose map.
      (dolist (position '(load 2 3))
        (let* ((forgejo-compose-mode-map (copy-keymap forgejo-compose-mode-map))
               (retained forgejo-compose-mode-map)
               (unrelated (cons 'forgejo-test-unrelated-mode (make-sparse-keymap)))
               (added (cons 'forgejo-test-added-mode (make-sparse-keymap)))
               (minor-mode-map-alist
                (cons unrelated (mapcar #'copy-sequence minor-mode-map-alist)))
               (loader (symbol-function 'load))
               (installer (symbol-function 'forgejo-dev--install-map))
               (last-file (expand-file-name (car (last files)) forgejo-test-dev--root))
               (count 0)
               caught)
          (setf (alist-get 'forgejo-compose-mode minor-mode-map-alist) retained)
          (let ((registry-entry (assq 'forgejo-compose-mode minor-mode-map-alist)))
            (define-key retained (kbd "C-c C-c") #'ignore)
            (with-temp-buffer
              (text-mode)
              (forgejo-compose-mode 1)
              (should (eq (key-binding (kbd "C-c C-c")) #'ignore))
              (condition-case condition
                  (cl-letf (((symbol-function 'load)
                             (lambda (file &rest args)
                               (prog1 (apply loader file args)
                                 (when (and (eq position 'load) (equal file last-file))
                                   (push added minor-mode-map-alist)
                                   (signal failure '("Injected late compose failure"))))))
                            ((symbol-function 'forgejo-dev--install-map)
                             (lambda (symbol old new)
                               (prog1 (funcall installer symbol old new)
                                 (when (eql (cl-incf count) position)
                                   (push added minor-mode-map-alist)
                                   (signal failure '("Injected compose publication failure")))))))
                    (forgejo-dev-load forgejo-test-dev--root files))
                ((error quit) (setq caught (car condition))))
              (should (eq caught failure))
              (should (eq forgejo-compose-mode-map retained))
              (should (eq (assq 'forgejo-compose-mode minor-mode-map-alist) registry-entry))
              (should (eq (cdr registry-entry) retained))
              (should (eq (key-binding (kbd "C-c C-c")) #'ignore))
              (should (eq (key-binding (kbd "C-c C-k")) #'forgejo-compose-abort))
              (should (eq (assq 'forgejo-test-unrelated-mode minor-mode-map-alist) unrelated))
              ;; Rollback owns only compose's entry, not the entire registry.
              (should (eq (assq 'forgejo-test-added-mode minor-mode-map-alist) added)))))))))

(ert-deftest forgejo-test-dev-compose-failure-restores-missing-dispatch ()
  (let ((files (forgejo-test-dev--sources)))
    (forgejo-dev-load forgejo-test-dev--root files)
    (dolist (failure '(error quit))
      (dolist (present '(nil t))
        (let* ((minor-mode-map-alist
                (assq-delete-all 'forgejo-compose-mode
                                 (mapcar #'copy-sequence minor-mode-map-alist)))
               (loader (symbol-function 'load))
               (last-file (expand-file-name (car (last files)) forgejo-test-dev--root))
               caught)
          (when present (push (list 'forgejo-compose-mode) minor-mode-map-alist))
          (with-temp-buffer
            (text-mode)
            (forgejo-compose-mode 1)
            (let ((binding (key-binding (kbd "C-c C-c"))))
              (condition-case condition
                  (cl-letf (((symbol-function 'load)
                             (lambda (file &rest args)
                               (prog1 (apply loader file args)
                                 (when (equal file last-file)
                                   (signal failure '("Injected absent dispatch failure")))))))
                    (forgejo-dev-load forgejo-test-dev--root files))
                ((error quit) (setq caught (car condition))))
              (should (eq caught failure))
              (should (equal (assq 'forgejo-compose-mode minor-mode-map-alist)
                             (and present '(forgejo-compose-mode))))
              (should (eq (key-binding (kbd "C-c C-c")) binding)))))))))

(provide 'forgejo-test-dev)
;;; forgejo-test-dev.el ends here
