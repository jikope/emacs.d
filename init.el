;; -*- lexical-binding: t; -*-

(setq user-full-name "Bima Wiratama")
(setq user-mail-address "bimakope@gmail.com")

(defvar jikope/default-font-size 105)
(defvar jikope/monospaced-font "Iosevka Mayukai Original")
(defvar jikope/variable-font "Roboto Condensed")

(load-theme 'modus-vivendi t)
(load-file "~/.emacs.d/lisp/startup.el")
(load-file "~/.emacs.d/lisp/ui.el")
(load-file "~/.emacs.d/lisp/evil.el")
(load-file "~/.emacs.d/lisp/utils.el") ; Project management and Emacs utilities
(load-file "~/.emacs.d/lisp/lsp.el")
(load-file "~/.emacs.d/lisp/prog.el")
(load-file "~/.emacs.d/lisp/keybindings.el")

(load-file "~/.emacs.d/lisp/email.el")

(with-eval-after-load 'org
  (load-file "~/.emacs.d/lisp/org.el"))

(use-package vterm
  :ensure t
  :after projectile
  :config
  ;;(setq vterm-keymap-exceptions (quote "C-j" "C-k" "C-l"))
  (define-key vterm-mode-map (kbd "C-l") nil)
  (define-key vterm-mode-map (kbd "C-j") nil)
  (define-key vterm-mode-map (kbd "C-h") nil)
  (define-key vterm-mode-map (kbd "C-k") nil)
  (define-key vterm-mode-map (kbd "<f5") nil)

  (define-key vterm-mode-map (kbd "C-l") #'windmove-right)
  (define-key vterm-mode-map (kbd "C-j") #'windmove-down)
  (define-key vterm-mode-map (kbd "C-h") #'windmove-left)
  (define-key vterm-mode-map (kbd "C-k") #'windmove-up)
  (define-key vterm-mode-map (kbd "<f5>") #'kill-this-buffer)

  (define-key vterm-mode-map (kbd "C-S-h") 'shrink-window-horizontally)
  (define-key vterm-mode-map (kbd "C-S-l") 'enlarge-window-horizontally)
  (define-key vterm-mode-map (kbd "C-S-j") 'shrink-window)
  (define-key vterm-mode-map (kbd "C-S-k") 'enlarge-window)

  (defun bima/open-split-term ()
    "Open Terminal and split Vertically"
    (interactive)
    (split-window-below)
    (windmove-down)
    (vterm "*Terminal*")
    (setq mode-line-format nil)
    (evil-emacs-state)
    )
  (global-set-key (kbd "<s-return>") 'bima/open-split-term))

;; (custom-set-variables
;;  '(magit-todos-keywords (list "TODO" "FIXME")))

;; (custom-set-faces
;;  '(evil-mc-region-face ((t (:inherit cursor))))
;;  '(org-modern-label ((t (:width normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-todos-keywords (list "TODO" "FIXME"))
 '(package-selected-packages
   '(afternoon-theme ample-theme olivetti vs-dark-theme eldoc-cmake vimish-fold zmq yasnippet-snippets wsd-mode writeroom-mode which-key web-mode vue-mode vterm vertico use-package undo-tree typescript-mode svg-tag-mode smartparens rjsx-mode ripgrep rainbow-delimiters pyvenv python-mode projectile prettier-js php-mode ox-twbs ox-reveal ox-gfm org-super-agenda org-roam-ui org-roam-bibtex org-ref org-present org-noter-pdftools org-modern org-download org-bullets orderless ob-restclient ob-mongo neotree navigel nano-theme mu4e-alert mood-line modern-cpp-font-lock mlscroll marginalia magit-todos lsp-pyright line-reminder json-navigator highlight-indent-guides helm-bibtex go-mode focus expand-region evil-mc evil-collection emmet-mode elisp-refs editorconfig doom-themes doom-modeline dired-posframe diminish dash-functional dap-mode consult company-box cmake-mode aggressive-indent add-node-modules-path)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-mc-region-face ((t (:inherit cursor))))
 '(org-modern-label ((t (:width normal)))))
