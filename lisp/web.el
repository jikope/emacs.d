(use-package web-mode
  :straight t
  :after magit
  :mode "\\.blade\\.php\\'"
  :config
					;(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
  (defun html-backend-hook ()
    "Hook for `web-mode'."
    (set (make-local-variable 'company-backends)
         '(company-web-html company-yasnippet company-files)))

  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.vue\\'" . prettier-js-mode))
  (add-hook 'vue-mode-hook 'html-backend-hook)

  (setq web-mode-engines-alist
        '(("php"    . "\\.html\\'")
          ("blade"  . "\\.blade\\."))
        )
  (setq-default indent-tabs-mode nil)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-indent-style 4))


(font-lock-add-keywords 'typescript-mode
                        `((,(concat
                             "\\<[_a-zA-Z][_a-zA-Z0-9]*\\>"       ; Object identifier
                             "\\s *"                              ; Optional white space
                             "\\(?:\\.\\|->\\)"                   ; Member access
                             "\\s *"                              ; Optional white space
                             "\\<\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\>" ; Member identifier
                             "\\s *"                              ; Optional white space
                             "(")                                 ; Paren for method invocation
                           1 'font-lock-function-name-face t)))

(use-package typescript-mode
  :straight t
  :mode
  ("\\.ts\\'" . typescript-mode)
  :hook
  ;(typescript-mode . prettier-js-mode)
  (typescript-mode . lsp-deferred)
  :commands (typescript-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (setq typescript-indent-level 4))

(use-package vue-mode
  :straight t
  :after magit
  :mode
  ("\\.vue\\'" . vue-mode))

(use-package add-node-modules-path
  :straight t
  :after magit)

(use-package php-mode
  :straight t
  :after magit
  :hook (php-mode . lsp))

(use-package js-mode
  :staright t
  :config
  (setq js-indent-level 4))

(use-package emmet-mode
  :straight t
  :hook ((svelte-mode web-mode) . emmet-mode)
  :config 
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  (define-key evil-insert-state-map (kbd "C-e") 'emmet-expand-line))

(use-package svelte-mode
  :straight t
  :after magit
  :config
  (setq svelte-basic-offset 4
        svelte-tag-relative-indent nil)
  (add-hook 'svelte-mode-hook (lambda () (electric-indent-mode -1))))
