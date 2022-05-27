(setq package-quickstart t) ; Newest Change
(setq package-enable-at-startup nil)
;;(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq site-run-file nil)
(setq byte-compile-warnings '(cl-functions)) ; remove cl warning
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq initial-frame-alist (quote ((fullscreen . maximized))))
(add-hook 'after-init-hook 'save-place-mode)
;; (setq initial-major-mode 'fundamental-mode)
(setq-default c-basic-offset 4)
(setq-default tab-width 4)
(setq-default line-spacing 7)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)
(menu-bar-mode -1)
(tool-bar-mode 0) ;;; Remove Toolbar
(scroll-bar-mode 0) ;;; Remove Scrollbar
(blink-cursor-mode 1) ;;; Remove Blink Cursor
(global-visual-line-mode t)

(global-prettify-symbols-mode +1)
(setq display-line-numbers-type 'relative)
(set-face-attribute 'line-number-current-line nil :weight 'bold)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq cursor-type 'box)

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)

(setq window-divider-default-right-width 1)
(setq window-divider-default-places 'right-only)
(window-divider-mode)

(set-face-attribute 'default 'nil :family "Iosevka Mayukai Original" :weight 'light :height jikope/default-font-size)
(set-face-attribute 'fixed-pitch 'nil :family jikope/monospaced-font :height jikope/default-font-size :weight 'light)
(set-face-attribute 'variable-pitch nil :font jikope/variable-font :height jikope/default-font-size :weight 'regular)


(use-package mlscroll
  :ensure t
  ;; :after doom-modeline
  :hook (after-init . mlscroll-mode)
  :config
  (require 'which-func)
  (setq mlscroll-shortfun-min-width 11))
;; (mlscroll-mode))
