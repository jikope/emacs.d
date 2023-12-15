(use-package undo-tree
  :straight t
  :hook (evil-mode . global-undo-tree-mode)
  :custom (undo-tree-auto-save-history nil))

(use-package evil
  :straight t
  :hook (after-init . evil-mode)
  :init 
  (setq evil-want-keybinding 'nil)
  :config
  (setq evil-undo-system 'undo-tree))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init '(dired calc calendar mu4e)))
