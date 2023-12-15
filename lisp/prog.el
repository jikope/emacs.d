(setq compilation-scroll-output 'first-error)
      ;compilation-window-height '50)

(add-hook 'c++-mode-hook '(lambda ()
                            (require 'dap-cpptools)))

;; (use-package treesit-auto
;;   :straight t
;;   :demand t
;;   :config
;;   (setq treesit-auto-install 'prompt)
;;   (global-treesit-auto-mode))

;; (use-package company-tabnine
;;   :straight t
;;   :after company
;;   :config
;;   (require 'company-tabnine)
;;   (add-to-list 'company-backends #'company-tabnine))

(use-package flycheck
  :straight t
  :defer t
  :hook (prog-mode . flycheck-mode))

;; (use-package ansi-color
;;   :straight t
;;   :after magit
;;   :config (progn 
;;             (defun my/ansi-colorize-buffer ()
;;               (let ((buffer-read-only nil))
;;                 (ansi-color-apply-on-region (point-min) (point-max))))
;;             (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)))

(use-package fancy-compilation
  :straight t
  :custom (fancy-compilation-override-colors . nil)
  :commands (fancy-compilation-mode))

(with-eval-after-load 'compile
  (fancy-compilation-mode))

(use-package yasnippet
  :straight t
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :after magit
  :config
  (with-eval-after-load 'evil
    (define-key evil-insert-state-map (kbd "C-r") 'yas-expand)))

(use-package yasnippet-snippets
  :straight t
  :after yasnipppet)

(use-package smartparens
  :straight t
  :after evil
  :diminish
  :hook ((prog-mode org-mode lisp-interaction-mode) . smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package expand-region
  :straight t
  :after evil
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package highlight-indent-guides
  :straight t
  :after magit
  :hook ((prog-mode web-mode sgml-mode nxml-mode) . 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-delay 0)
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-character-face-perc 7)

  (add-hook 'highlight-indent-guides-mode-hook
            (lambda ()
              (set-face-foreground 'highlight-indent-guides-character-face "gray21")
              (set-face-foreground 'highlight-indent-guides-top-character-face "dimgray")
              ))

  (eval-after-load 'highlight-indent-guides
    (highlight-indent-guides-auto-set-faces)
    (set-face-attribute 'highlight-indent-guides-character-face nil :font "Inconsolata")
    (set-face-attribute 'highlight-indent-guides-top-character-face nil :font "Inconsolata")))

(use-package rainbow-delimiters
  :straight t
  :after magit
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package evil-mc
  :straight t
  :after magit
  :hook
  ((prog-mode org-mode) . evil-mc-mode)
  :init
  (define-key evil-normal-state-map (kbd "C-c <return>") 'evil-mc-undo-all-cursors)
  (global-set-key (kbd "C-S-<mouse-1>") 'evil-mc-toggle-cursor-on-click))

(use-package origami
  :straight t
  :after magit
  :hook (prog-mode . origami-mode))

;; (use-package aggressive-indent
  ;; :straight t
  ;; :after magit
  ;; :hook (org-mode . aggressive-indent-mode)
  ;; :config
  ;; (add-hook 'prog-mode-hook (lambda ()
  ;;                             (unless (eq major-mode 'c++-mode)
  ;;                               (aggressive-indent-mode)))))

(use-package cmake-mode
  :straight t
  :after magit)

;; (use-package eldoc-cmake
;;   :after magit
;;   :hook (cmake-mode . eldoc-cmake-enable))

(use-package clang-format+
  :straight t
  :after magit)

(use-package editorconfig
  :straight t
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

(use-package protobuf-mode
  :straight t
  :after magit)

(use-package lua-mode
  :straight t
  :after magit)

(use-package python-mode
  :straight t
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
  :straight t
  :after python-mode
  :config
  (setq pyvenv-menu t)
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
                                          (message "VENV activated")
					                                (pyvenv-restart-python)))
  :hook (python-mode . pyvenv-mode))

(use-package lsp-pyright
  :straight t
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

(use-package go-mode
  :straight t
  :after magit)

;; RUST
(use-package rust-mode
  :straight t
  :after magit
  :hook (rust-mode . tree-sitter-mode)
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (require 'dap-cpptools)
              ;; installs .extension/vscode
              (dap-register-debug-template "Rust::CppTools Run Configuration"
                                           (list :type "cppdbg"
                                                 :request "launch"
                                                 :name "Rust::Run"
                                                 :MIMode "gdb"
                                                 :miDebuggerPath "rust-gdb"
                                                 :environment []
                                                 :program "${workspaceFolder}/target/debug/hello / replace with binary"
                                                 :cwd "${workspaceFolder}"
                                                 :console "external"
                                                 :dap-compilation "cargo build"
                                                 :dap-compilation-dir "${workspaceFolder}"))
              (prettify-symbols-mode)
              (yas-minor-mode))))

(use-package wgsl-mode
  :straight t
  :after rust-mode)

;; Odin
(use-package odin-mode
  :straight (odin-mode :type git :host github :repo "mattt-b/odin-mode")
  :after magit)
