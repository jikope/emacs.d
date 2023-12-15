(use-package lsp-mode
  :straight t
  :after magit
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :hook (prog-mode . lsp)
  ;; :hook (c++-mode . lsp)
  :bind ("C-c f a" . lsp-format-buffer)
  :config
  (setq lsp-auto-guess-root t
        lsp-log-io nil
        lsp-restart 'auto-restart
        lsp-enable-symbol-highlighting nil
        lsp-enable-on-type-formatting nil
        lsp-signature-auto-activate nil
        lsp-signature-render-documentation t
        lsp-eldoc-hook nil
        lsp-inlay-hint-enable nil ; INLAY HINT
        lsp-semantic-tokens-enable nil
        lsp-enable-folding nil
        lsp-enable-imenu nil
        lsp-enable-snippet nil
        lsp-enable-file-watchers nil
        lsp-headerline-breadcrumb-enable nil
        lsp-headerline-breadcrumb-segments nil
        lsp-lens-enable nil
        read-process-output-max (* 1024 1024)
        lsp-keep-workspace-alive nil
        lsp-idle-delay 0.5)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]data$")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\].git$")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\].submodules$")
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "g d r") 'lsp-find-references)
    (define-key evil-normal-state-map (kbd "g d j") 'lsp-find-definition))
  :bind (("C-c h" . lsp-describe-thing-at-point)))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.extern\\'"))

(use-package dap-mode
  :straight t
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

;; (use-package realgud
;;   :straight t)

;; (use-package lsp-bridge
;;   :after yasnippet
;;   :load-path "~/.emacs/plugins/lsp-brigde"
;;   :hook (prog-mode . lsp-bridge-mode))
