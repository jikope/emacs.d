(use-package vimish-fold
  :ensure t
  :hook (prog-mode . vimish-fold-mode))

(use-package flycheck
  :ensure t
  :defer t
  :hook (prog-mode . flycheck-mode))

(use-package yasnippet
  :ensure t
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :config
  (with-eval-after-load 'evil
    (define-key evil-insert-state-map (kbd "C-r") 'yas-expand)))

(use-package yasnippet-snippets
  :ensure t
  :after yasnipppet)

(use-package smartparens
  :ensure t
  :after projectile
  :hook ((prog-mode org-mode lisp-interaction-mode) . smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package expand-region
  :ensure t
  :after magit
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package highlight-indent-guides
  :ensure t
  :after magit
  :hook ((prog-mode web-mode sgml-mode nxml-mode) . 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-delay 0)
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-character-face-perc 7)
  (eval-after-load 'highlight-indent-guides
    (highlight-indent-guides-auto-set-faces)))

(use-package rainbow-delimiters
  :ensure t
  :after magit
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package evil-mc
  :ensure t
  :after evil
  :hook
  ((prog-mode org-mode) . evil-mc-mode)
  :config
  (define-key evil-normal-state-map (kbd "C-c <return>") 'evil-mc-undo-all-cursors)
  (global-set-key (kbd "C-S-<mouse-1>") 'evil-mc-toggle-cursor-on-click))

(use-package aggressive-indent
  :ensure t
  :after magit
  :hook (org-mode . aggressive-indent-mode)
  :config
  (add-hook 'prog-mode-hook (lambda ()
                              (unless (eq major-mode 'c++-mode)
                                (aggressive-indent-mode)))))

(use-package eldoc-cmake
  :after magit
  :hook (cmake-mode . eldoc-cmake-enable))

(use-package editorconfig
  :ensure t
  :after magit
  :hook (prog-mode . editorconfig-mode))

(font-lock-add-keywords 'python-mode
                        `((,(concat
			                       "\\<[_a-zA-Z][_a-zA-Z0-9]*\\>"       ; Object identifier
			                       "\\s *"                              ; Optional white space
			                       "\\(?:\\.\\|->\\)"                   ; Member access
			                       "\\s *"                              ; Optional white space
			                       "\\<\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\>" ; Member identifier
			                       "\\s *"                              ; Optional white space
			                       "(")                                 ; Paren for method invocation
		                       1 'font-lock-function-name-face t)))

(use-package python-mode
  :ensure t
  :after magit
  :bind (:map python-mode-map
              ("C-<backspace>" . 'backward-kill-word)
              ("C-j" . 'windmove-down)
              ("C-c C-c" . 'recompile)
              ("C-c C-l" . 'python-shell-send-file))
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package pyvenv
  :ensure t
  :after python-mode
  :config
  (setq pyvenv-menu t)
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
                                          (message "VENV activated")
					                                (pyvenv-restart-python)))
  :hook (python-mode . pyvenv-mode))

(use-package lsp-pyright
  :ensure t
  :after lsp
  :config
  ;;(setq lsp-clients-python-library-directories '("/usr/" "~/miniconda3/pkgs"))
  (setq lsp-clients-python-library-directories '("/usr/"
                                                 "/media/data/Programming/Fundamental_of_Music_Processing/FMP/"))
  (setq lsp-pyright-disable-language-service nil
	      lsp-pyright-disable-organize-imports nil
	      lsp-pyright-auto-import-completions t
	      lsp-pyright-use-library-code-for-types t)
  ;;lsp-pyright-venv-path "~/miniconda3/envs")
  :hook ((python-mode . (lambda () 
                          (require 'lsp-pyright) (lsp-deferred)))))
