;; ;(setq package-quickstart t) ; Newest Change
;; (menu-bar-mode -1)
;; (tool-bar-mode 0) ;;; Remove Toolbar
;; (scroll-bar-mode 0) ;;; Remove Scrollbar
;; (blink-cursor-mode 1) ;;; Remove Blink Cursor
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

;; (set-frame-font jikope/monospaced-font nil t)
(defun bima/set-fonts-init ()
  (recentf-mode)
  (set-face-attribute 'default 'nil :family jikope/monospaced-font :weight 'regular :height jikope/default-font-size)
  ;; (set-face-attribute 'fixed-pitch 'nil :family jikope/monospaced-font :height jikope/default-font-size :weight 'regular)
  (set-face-attribute 'fixed-pitch 'nil :font jikope/monospaced-font)
  ;; (set-face-attribute 'italic nil
  ;;                     :weight 'regular
  ;;                     :slant 'oblique
  ;;                     :family "Victor Mono")
  ;;:height jikope/default-font-size)
  (set-face-attribute 'variable-pitch nil :font jikope/variable-font :height (* jikope/variable-font-size 10) :weight 'light))

;; This makes init faster
(add-hook 'after-init-hook 'bima/set-fonts-init)

;; (setq modus-themes-tabs-accented t
;;       modus-themes-italic-constructs t
;;       modus-themes-bold-constructs nil
;;       modus-themes-region '(no-extend)
;;       modus-themes-lang-checkers '(text-also straight-underline)
;;       modus-themes-links '(italic faint)
;;       modus-themes-hl-line '(intense)
;;       modus-themes-paren-match '(accented intense))
;; (setq modus-themes-prompts '(intense bold))

;; (setq modus-themes-completions '((matches . (extrabold intense))
;;                                  (selection . (underline))
;;                                  (popup . (intense))))
(setq modus-themes-org-blocks 'gray-background)
;; (setq modus-themes-org-agenda '((header-block . (variable-pitch 1.2))
;;                                 (habit . traffic-light)))
;; (setq modus-themes-headings '((t . (rainbow))))

(setq modus-vivendi-palette-overrides '((bg-main "#181818")
                                        ;(fg-main "#f1f1f1")
                                        (fg-main "#f8f8f2")
                                        (fg-line-number-inactive fg-main)
                                        (bg-line-number-inactive bg-main)
                                        (fg-line-number-active fg-main-intense)
                                        (bg-line-number-active bg-active)
                                        (fg-heading-1 magenta-cooler)))

(setq modus-operandi-palette-overrides'((bg-main "#f7f7f8")
                                        (fg-main "#202123")
                                        (fg-line-number-inactive fg-main)
                                        (bg-line-number-inactive bg-main)
                                        (fg-line-number-active fg-main-intense)
                                        (bg-line-number-active bg-active)
                                        (fg-heading-1 magenta-cooler)))

;; Org Faces
(with-eval-after-load 'org-ref
  (set-face-attribute 'org-ref-cite-face nil :inherit 'default :weight 'regular :foreground "SeaGreen3")
  (set-face-attribute 'org-ref-ref-face nil :inherit 'default :weight 'regular :foreground "IndianRed1"))

(with-eval-after-load 'diredfl
  (set-face-attribute 'diredfl-dir-name nil :inherit 'default :weight 'regular))



;; (use-package mini-modeline
;;   :straight t
;;   :diminish
;;   :hook (after-init . mini-modeline-mode))

;; (use-package shrink-path
;;   :straight t)

;; (use-package doom-modeline
;;   :straight t
;;   :hook (after-init . doom-modeline-mode)
;;   :defer t
;;   :config
;;   (setq doom-modeline-bar-width 2)
;;   (setq doom-modeline-icon nil)
;;   (setq doom-modeline-modal-icon nil)
;;   (setq doom-modeline-height 20)
;;   (setq doom-modeline-buffer-file-name-style 'relative-from-project)
;;   (set-face-attribute 'mode-line nil :height 80 :family "Roboto Mono" :weight 'light)
;;   (set-face-attribute 'mode-line-inactive nil :height 80))

(use-package mood-line
  :straight t
  :hook (after-init . mood-line-mode))

;; (use-package posframe
;;   :straight t)

;; (use-package awesome-tray
;;   :straight nil
;;   :load-path "~/.emacs.d/plugins/awesome-tray"
;;   :hook (after-init . awesome-tray-mode)
;;   :config
;;   (setq awesome-tray-buffer-name-max-length '50)
;;   (setq awesome-tray-active-modules '("circe" "org-pomodoro" "evil" "location" "buffer-read-only" "file-path" "buffer-name" "git" "mode-name" "pdf-view-page" "flymake")))

;; (use-package mlscroll
;;   :straight t
;;   :after mood-line
;;   ;; :hook (doom-modeline-mode . mlscroll-mode)
;;   :config
;;   (require 'which-func)
;;   (setq mlscroll-shortfun-min-width 11)
;;   (mlscroll-mode))
