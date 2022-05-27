(use-package lsp-mode
  :ensure t
  :after magit
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :hook (prog-mode . lsp)
  :bind ("C-c f a" . lsp-format-buffer)
  :config
  (require 'lsp-mode)
  (setq lsp-auto-guess-root t
        lsp-log-io nil
        lsp-restart 'auto-restart
        lsp-enable-symbol-highlighting nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-on-type-formatting nil
        lsp-signature-auto-activate nil
        lsp-signature-render-documentation nil
        lsp-eldoc-hook nil
        lsp-semantic-tokens-enable nil
        lsp-enable-folding nil
        lsp-enable-imenu nil
        lsp-enable-snippet nil
        lsp-enable-file-watchers nil
        lsp-headerline-breadcrumb-enable nil
        read-process-output-max (* 1024 1024)
        lsp-keep-workspace-alive nil
        lsp-idle-delay 0.5)

  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "g d j") 'lsp-find-definition)))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-auto-configure-mode))
