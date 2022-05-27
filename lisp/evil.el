(use-package undo-tree
  :ensure t
  :hook (evil-mode . global-undo-tree-mode))

(use-package evil
  :ensure t
  :hook (emacs-startup . evil-mode)
  :init 
  (setq evil-want-keybinding 'nil)
  :config
  (setq evil-undo-system 'undo-tree))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init '(dired calc calendar mu4e)))
