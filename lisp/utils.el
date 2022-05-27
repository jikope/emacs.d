;; ------------------------------------------------------------
;; PROJECT MANAGEMENT 
;; ------------------------------------------------------------
(use-package projectile
  :ensure t
  :hook (after-init . projectile-mode)
  :config 
  ;; (setq projectile-project-search-path '("/media/data/Web-Applications"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package ripgrep
  :ensure t
  :after projectile)

(use-package magit
  :ensure t
  :defer 10)

(use-package magit-todos
  :ensure t
  :after magit
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-recursive t
        magit-todos-depth 10
        magit-todos-exclude-globs `(".git" ".log" "node_modules" "elpa" "plugins"))
  (custom-set-variables
   '(magit-todos-keywords (list "TODO" "FIXME"))))

;; TODO https://github.com/Artawower/blamer.el
(use-package neotree
  :ensure t
  :after projectile
  :config
  (setq neo-theme (if (display-graphic-p) 'icons ))
  (setq neo-smart-open t)
  (setq neo-window-width 32)
  (defun neotree-set-font ()
    "Set font size of neotree"
    (setq buffer-face-mode-face '(:family "Roboto Condensed" :height 100))
    (buffer-face-mode)
    (visual-line-mode -1))
  (add-hook 'neotree-mode-hook #'neotree-set-font)
  (define-key neotree-mode-map (kbd "<tab>") 'neotree-enter)
  (define-key neotree-mode-map (kbd "C-c t h") 'neotree-hidden-file-toggle))

;; ------------------------------------------------------------
;; COMPLETION SYSTEM 
;; ------------------------------------------------------------
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (define-key vertico-map (kbd "C-j") #'vertico-insert)
  (define-key vertico-map (kbd "C-l") #'backward-kill-word)
  (global-set-key (kbd "C-;") 'execute-extended-command)
  ;; grow and shrink the vertico minibuffer
  (setq vertico-resize t)

  ;; optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

(use-package orderless
  :ensure t
  :after vertico
  :config
  (setq completion-styles '(orderless)
	      completion-category-defaults nil
	      completion-category-overrides '((file (styles partial-completion)))))

(use-package emacs
  :init
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
	      '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)

  (setq read-extended-command-predicate #'command-completion-default-include-p)
  (setq enable-recursive-minibuffers t))

(use-package marginalia
  :ensure t
  :after vertico
  :hook (vertico-mode . marginalia-mode)
  :bind (("M-A" . marginalia-cycle)
	       :map minibuffer-local-map
	       ("M-A" . marginalia-cycle)))

(use-package consult
  :ensure t
  :after vertico
  :bind (;; C-c bindings (mode-specific-map)
	       ("C-x f" . consult-recent-file)
	       ("C-x b" . consult-buffer)
	       ("C-c p r" . consult-ripgrep)
	       ("C-s" . consult-line))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (recentf-mode)
  (setq register-preview-delay 0
	      register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (setq xref-show-xrefs-function #'consult-xref
	      xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
                                        ;consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  (setq consult-narrow-key "<") ;; (kbd "C-+")

  (setq consult-project-root-function
	      (lambda ()
	        (when-let (project (project-current))
	          (car (project-roots project))))))

;; ------------------------------------------------------------
;; EMACS UTILITIES 
;; ------------------------------------------------------------
(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  (define-key which-key-mode-map (kbd "<escape>") 'keyboard-escape-quit))

(use-package corfu
  :ensure t
  :after vertico
  :hook ((prog-mode org-mode) . corfu-mode)
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  (corfu-scroll-margin 5)        ;; Use scroll margin

  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
