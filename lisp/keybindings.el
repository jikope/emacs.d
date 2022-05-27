(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "C-n") 'right-char)
  (define-key evil-insert-state-map (kbd "C-p") 'left-char))

(add-hook 'after-init-hook
          (lambda ()
            ;; Load custom function
            (load-file "~/.emacs.d/custom-functions.el")
            (global-set-key (kbd "<f5>") 'kill-this-buffer)
            (global-set-key (kbd "S-<f5>") 'jikope/kill-and-close-window)
            (global-set-key (kbd "<f6>") 'eval-buffer)
            (global-set-key (kbd "<C-x r C-d>") 'writeroom-decrease-width)
            (global-set-key (kbd "<C-x r C-i>") 'writeroom-increase-width)
            (global-set-key (kbd "C-c p r") 'consult-ripgrep)
                                        ;(add-hook 'dired-mode-hook #'evil-emacs-state)

            ;; WINDOW MOVEMENT
            (global-set-key (kbd "C-k") 'windmove-up)
            (global-set-key (kbd "C-j") 'windmove-down)
            (global-set-key (kbd "C-h") 'windmove-left)
            (global-set-key (kbd "C-l") 'windmove-right)

            (global-set-key (kbd "C-S-h") 'shrink-window-horizontally)
            (global-set-key (kbd "C-S-l") 'enlarge-window-horizontally)
            (global-set-key (kbd "C-S-j") 'shrink-window)
            (global-set-key (kbd "C-S-k") 'enlarge-window)

            ;; GLOBAL KEYBINDINGS
            (global-set-key (kbd "C-c a") 'org-agenda)
            (global-set-key (kbd "C-c c") 'org-capture)
            (global-set-key (kbd "C-c l") 'org-store-link)

            (global-set-key (kbd "<f3>") 'neotree-toggle)
            (global-set-key (kbd "C-x g") 'magit-status)

            (custom-set-faces
             '(evil-mc-region-face ((t (:inherit cursor)))))))
