(setq gc-cons-threshold 20000000) ; 2^61 bytes

;; From doom emacs FAQ
(defun doom-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
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

(add-hook 'after-init-hook
          (lambda ()
            (require 'package)
            (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
            (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
            (package-initialize)

            ;; Install use package and update repositories if use-package not installed
            (when (not (package-installed-p 'use-package))
              (package-refresh-contents)
              (package-install 'use-package))
            (require 'use-package)))
