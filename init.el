;;-*- lexical-binding: t; -*-
(setq user-full-name "Bima Wiratama")
(setq user-mail-address "bimakope@gmail.com")

;; Turn off all warnings
(setq warning-minimum-level :emergency)
(setq native-comp-async-report-warnings-errors nil)
;; (setq warning-suppress-log-types '((package reinitialization)))  (package-initialize) ; Too Slow!

(defvar jikope/default-font-size 135)
(defvar jikope/variable-font-size 15)
(defvar jikope/svg-tag-size 18)
(defvar jikope/svg-tag-height 0.75)
(defvar jikope/monospaced-font "Roboto Mono")
;; (defvar jikope/monospaced-font "M PLUS 1p")
;; (defvar jikope/variable-font "Gentium Plus")
(defvar jikope/variable-font "Roboto Mono")
;; (defvar jikope/variable-font "Anek Bangla")

;; Colors
(defvar jikope/background-dark "#0c0e14") ; Dark
(defvar jikope/background-light "#e8edf3") ; Light

(load-file "~/.emacs.d/lisp/startup.el")
(load-file "~/.emacs.d/lisp/defaults.el")
(load-file "~/.emacs.d/lisp/ui.el")

(use-package emacsql
  :after magit
  :load-path "~/.emacs.d/plugins/emacsql")

(straight-use-package 'org)

(with-eval-after-load 'org
  (load-file "~/.emacs.d/lisp/org.el"))

(load-file "~/.emacs.d/lisp/evil.el")
(load-file "~/.emacs.d/lisp/utils.el") ; Project management and Emacs utilities
(load-file "~/.emacs.d/lisp/fileman.el")
(load-file "~/.emacs.d/lisp/lsp.el")
(load-file "~/.emacs.d/lisp/prog.el")
(load-file "~/.emacs.d/lisp/web.el")
(load-file "~/.emacs.d/lisp/keybindings.el")
;(load-file "~/.emacs.d/lisp/email.el")

;; (use-package modus-themes
;;    :straight t)

;; (load-theme 'modus-vivendi t)

(use-package vscode-dark-plus-theme
  :load-path "~/.emacs.d/plugins/vscode-dark-plus-emacs-theme"
  :config
  (setq vscode-dark-plus-box-org-todo nil)
  (setq vscode-dark-plus-scale-org-faces nil)
  (setq vscode-dark-plus-invert-hl-todo nil)
  (load-theme 'vscode-dark-plus t))

(use-package vterm
  :straight t
  :after projectile
  :config
  ;; (define-key vterm-mode-map (kbd "C-l") nil)
  ;; (define-key vterm-mode-map (kbd "C-j") nil)
  ;; (define-key vterm-mode-map (kbd "C-h") nil)
  ;; (define-key vterm-mode-map (kbd "C-k") nil)
  ;; (define-key vterm-mode-map (kbd "<f5") nil)

  (define-key vterm-mode-map (kbd "C-l") #'windmove-right)
  (define-key vterm-mode-map (kbd "C-j") #'windmove-down)
  (define-key vterm-mode-map (kbd "C-h") #'windmove-left)
  (define-key vterm-mode-map (kbd "C-k") #'windmove-up)
  (define-key vterm-mode-map (kbd "<f5>") #'kill-this-buffer)

  (defun bima/open-split-term ()
    "Open Terminal and split Vertically"
    (interactive)
    (split-window-below)
    (windmove-down)
    (vterm "*Terminal*")
    (setq mode-line-format nil)
    (evil-emacs-state))
  (global-set-key (kbd "<s-return>") 'bima/open-split-term))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "5a611788d47c1deec31494eb2bb864fde402b32b139fe461312589a9f28835db" default))
 '(magit-todos-keywords (list "TODO" "FIXME")))
;; '(package-selected-packages

(custom-set-faces
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
;; '(default ((t (:background "#0c0e14")))) ; Dark
;; '(default ((t (:background "#0b0b0f")))) ; Dark
'(mode-line ((t (:font "Iosevka" :box '(:width 1 :color "#a8a8a8") :height 120 :background nil :foreground nil))))

'(mode-line-inactive ((t (:font "Iosevka" :height 120 :background nil :foreground nil))))
;; '(mode-line ((t (:font "Iosevka" :height 120 :background nil))))
'(fringe ((t (:background nil))))

'(evil-mc-region-face ((t (:inherit cursor))))
;; '(header-line ((t (:height 200 :foreground nil :background nil))))
'(org-modern-label ((t (:width normal)))))
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
