;(setq gc-cons-threshold 20000000) ; 2^61 bytes

(setq gc-cons-threshold most-positive-fixnum
	  gc-cons-percentage 0.1
      inhibit-default-init t
      package-enable-at-startup nil
      straight-check-for-modifications nil)


;; From doom emacs FAQ
(defun doom-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  ;; Defer it so that ommands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

(defun bima/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
	                 (float-time
	                  (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'bima/display-startup-time)


(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; Maximized startup
;; (setq-default
;;  inhibit-startup-screen t               ; Disable start-up screen
;;  inhibit-startup-message t              ; Disable startup message
;;  inhibit-startup-echo-area-message t    ; Disable initial echo message
;;  initial-scratch-message ""             ; Empty the initial *scratch* buffer
;;  initial-buffer-choice t)               ; Open *scratch* buffer at init

(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding
(prefer-coding-system       'utf-8)     ; Add utf-8 at the front for automatic detection.
(set-terminal-coding-system 'utf-8)     ; Set coding system of terminal output
(set-keyboard-coding-system 'utf-8)     ; Set coding system for keyboard input on TERMINAL
(set-language-environment "English")    ; Set up multilingual environment

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(blink-cursor-mode . 1) default-frame-alist)

(setq backup-directory-alist       ; File name patterns and backup directory names.
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      make-backup-files nil          ; Backup of a file the first time it is saved.
      vc-make-backup-files nil       ; No backup of files under version contr
      backup-by-copying t          ; Don't clobber symlinks
      version-control t            ; Version numbers for backup files
      delete-old-versions t        ; Delete excess backup files silently
      kept-old-versions 3          ; Number of old versions to keep
      kept-new-versions 6          ; Number of new versions to keep
      delete-by-moving-to-trash nil ; Delete files to trash
      history-delete-duplicates t)

(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth '(1 single-branch))  ;; instead of the default 'full
(setq use-package-verbose nil)
