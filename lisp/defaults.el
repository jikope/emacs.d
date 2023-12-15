(setq file-name-handler-alist nil
      site-run-file nil
      auto-save-default nil
      create-lockfiles nil)

(setq frame-inhibit-implied-resize t)

(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-x-resources t)
(setq inhibit-default-init t)
(setq inhibit-compacting-font-caches t)

;; Pixelwise resize windows
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

(setq byte-compile-warnings '(cl-functions)) ; remove cl warning
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(add-hook 'after-init-hook 'save-place-mode)
;; (setq initial-major-mode 'fundamental-mode)
(setq-default c-basic-offset 4
              tab-width 4
              line-spacing 2
              ;line-spacing 6
              evil-shift-width tab-width
              indent-tabs-mode nil
              sentence-end-double-space nil           ; Use a single space after dots
              bidi-paragraph-direction 'left-to-right ; Faster
              truncate-string-ellipsis "…")           ; Nicer ellipsis

(setq shell-file-name "/usr/bin/fish")
;(add-to-list 'browse-url-firefox-arguments "-private-window")
