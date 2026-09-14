;;; forgejo-dev.el --- Development checks and source reload -*- lexical-binding: t; -*-

;;; Commentary:

;; Developer tooling, not an installed library.  Reload is bounded to the
;; declared package maps: it does not roll back functions or arbitrary Lisp
;; side effects.  Existing map objects stay alive for retained buffers,
;; text properties and minor-mode maps.  Options are never unbound.

;;; Code:

(require 'cl-lib)

(defconst forgejo-dev--maps
  '(forgejo-map forgejo-repo-action-map forgejo-compose-mode-map
    forgejo-tl-list-mode-map forgejo-view-action-map forgejo-view-diff-map
    forgejo-view-mode-map forgejo-view-edit-map forgejo-repo-search-mode-map
    forgejo-issue-list-mode-map forgejo-issue-view-mode-map
    forgejo-pull-list-mode-map forgejo-pull-log-map forgejo-pull-view-mode-map
    forgejo-vc-map forgejo-review-thread-map forgejo-watch-list-mode-map
    forgejo-notification-list-mode-map)
  "Static maps rebuilt by the source reload.
Dynamically populated settings maps are deliberately retained.")

(defvar forgejo-dev--defaults nil
  "Map defaults from the last successful development reload.")

(defun forgejo-dev--bindings (map)
  "Return MAP's own bindings, without its parent or popup metadata."
  (when (keymapp map)
    (let ((copy (copy-keymap map)) bindings)
      (set-keymap-parent copy nil)
      (map-keymap (lambda (key binding)
                   (unless (eq key 'keymap-popup)
                     (push (cons key binding) bindings)))
                 copy)
      bindings)))

(defun forgejo-dev--preserve-bindings (old new defaults)
  "Apply edits from OLD to NEW relative to DEFAULTS.
Without DEFAULTS, conservatively preserve every existing binding."
  (let ((bindings (forgejo-dev--bindings old))
        (baseline (forgejo-dev--bindings defaults)))
    (dolist (key (delete-dups (append (mapcar #'car bindings)
                                    (mapcar #'car baseline))))
      (let ((binding (alist-get key bindings)))
        (unless (and defaults (equal binding (alist-get key baseline)))
          (define-key new (vector key) binding))))))

(defun forgejo-dev--install-map (symbol old new)
  "Install NEW as SYMBOL while keeping OLD's identity when it is a map."
  (when (keymapp old)
    (setcdr old (cdr new))
    (set symbol old)))

(defun forgejo-dev-load (root files)
  "Reload source FILES relative to ROOT, preserving retained package maps.
Errors and quits restore map bindings, compose dispatch and buffer maps,
then propagate.
Functions already loaded are NOT rolled back; fix the source and retry.
On the first reload existing bindings win; later reloads update unchanged
build defaults while preserving key edits made since the preceding reload."
  (let* ((paths (mapcar (lambda (file) (expand-file-name file root)) files))
         (saved (mapcar (lambda (symbol)
                          (let ((bound (boundp symbol)))
                            (list symbol bound (and bound (symbol-value symbol)))))
                        forgejo-dev--maps))
         (buffers (mapcar (lambda (buffer)
                            (cons buffer (with-current-buffer buffer (current-local-map))))
                          (buffer-list)))
         ;; Save the original tails, not copies: restoration keeps identity.
         (tails (cl-loop for (_ _ map) in saved when (keymapp map)
                         collect (cons map (cdr map))))
         (old-load-path load-path)
         (old-defaults forgejo-dev--defaults)
         ;; `define-minor-mode' mutates this dispatch entry when utils reloads.
         (compose-entry (assq 'forgejo-compose-mode minor-mode-map-alist))
         (compose-map (cdr compose-entry))
         ;; forgejo-vc installs this public binding at load time.  Keep a
         ;; pre-existing user binding (including an explicit removal after
         ;; the package was loaded), while allowing first-time installation.
         (vc-map (and (boundp 'vc-prefix-map) (symbol-value 'vc-prefix-map)))
         (vc-key (and vc-map (lookup-key vc-map "f")))
         (preserve-vc-key (or vc-key (featurep 'forgejo-vc)))
         success)
    (dolist (path paths)
      (unless (file-readable-p path) (error "Unreadable source: %s" path)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (dolist (symbol forgejo-dev--maps)
          (when (local-variable-p symbol)
            (error "Cannot reload buffer-local map: %s" symbol)))
        (when (and buffer-file-name (buffer-modified-p)
                   (member (file-truename buffer-file-name)
                           (mapcar #'file-truename paths)))
          (error "Save modified source buffer first: %s" (buffer-name)))))
    (dolist (symbol forgejo-dev--maps)
      (when (get-variable-watchers symbol)
        (error "Cannot reload watched map: %s" symbol)))
    (unwind-protect
        (progn
          (add-to-list 'load-path (expand-file-name "lisp" root))
          (dolist (symbol forgejo-dev--maps) (makunbound symbol))
          (dolist (path paths) (load path nil t t))
          (let* ((fresh (mapcar (lambda (symbol)
                                 (cons symbol (symbol-value symbol)))
                               forgejo-dev--maps))
                 (defaults (mapcar (lambda (entry)
                                     (cons (car entry) (copy-keymap (cdr entry))))
                                   fresh)))
            ;; Resolve rebuilt parents back to their retained objects before
            ;; publishing, so every old buffer sees updated parent bindings.
            (dolist (entry fresh)
              (let* ((map (cdr entry))
                     (parent (keymap-parent map))
                     (parent-symbol (car (rassq parent fresh)))
                     (old-parent (nth 2 (assq parent-symbol saved)))
                     (old (nth 2 (assq (car entry) saved))))
                (when (keymapp old-parent) (set-keymap-parent map old-parent))
                ;; A separately installed parent is user-owned, not a default
                ;; to discard when rebuilding package maps.
                (when (and (keymapp old) (keymap-parent old)
                           (not (memq (keymap-parent old) (mapcar #'caddr saved))))
                  (set-keymap-parent map (keymap-parent old)))
                (when (keymapp old)
                  (forgejo-dev--preserve-bindings
                   old map (alist-get (car entry) forgejo-dev--defaults)))))
            (dolist (entry fresh)
              (forgejo-dev--install-map
               (car entry) (nth 2 (assq (car entry) saved)) (cdr entry)))
            (setf (alist-get 'forgejo-compose-mode minor-mode-map-alist)
                  (symbol-value 'forgejo-compose-mode-map))
            (setq forgejo-dev--defaults defaults))
          (setq success t))
      (unless success
        (let ((inhibit-quit t))
          (dolist (entry tails) (setcdr (car entry) (cdr entry)))
          (dolist (entry saved)
            (if (nth 1 entry) (set (car entry) (nth 2 entry))
              (makunbound (car entry))))
          (if compose-entry
              (setf (alist-get 'forgejo-compose-mode minor-mode-map-alist) compose-map)
            (setq minor-mode-map-alist
                  (assq-delete-all 'forgejo-compose-mode minor-mode-map-alist)))
          (dolist (entry buffers)
            (when (buffer-live-p (car entry))
              (with-current-buffer (car entry) (use-local-map (cdr entry)))))
          (setq load-path old-load-path
                forgejo-dev--defaults old-defaults)))
      (when (and vc-map (or preserve-vc-key (not success)))
        (let ((inhibit-quit t)) (define-key vc-map "f" vc-key))))
    t))

(provide 'forgejo-dev)
;;; forgejo-dev.el ends here
