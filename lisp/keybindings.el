(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "C-n") 'right-char)
  (define-key evil-insert-state-map (kbd "C-p") 'left-char))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "SPC") #'company-abort))

(add-hook 'after-init-hook
          (lambda ()
            ;; Load custom function
            (load-file "~/.emacs.d/custom-functions.el")
            (global-set-key (kbd "C-,") 'compile)
            (global-set-key (kbd "C-<") 'kill-line)
            (global-set-key (kbd "<f6>") 'eval-buffer)
            (global-set-key (kbd "C-:") 'shell-command)
            (global-set-key (kbd "S-<f6>") 'eval-region)
            (global-set-key (kbd "<f5>") 'kill-this-buffer)
            (global-set-key (kbd "C-c p r") 'consult-ripgrep)
            (global-set-key (kbd "C-x 9") 'delete-other-windows)
            (global-set-key (kbd "S-<f5>") 'jikope/kill-and-close-window)
            (global-set-key (kbd "<C-x r C-d>") 'writeroom-decrease-width)
            (global-set-key (kbd "<C-x r C-i>") 'writeroom-increase-width)
            (global-set-key (kbd "C-0") 'evil-switch-to-windows-last-buffer)
            (global-set-key (kbd "C-<return>") 'evil-find-file-at-point-with-line)

            (with-eval-after-load 'org
              ;; (define-key org-mode-map (kbd "C-c y") #'org-retrieve-url-from-point)
              (define-key org-mode-map (kbd "C-c y") #'farynaio/org-link-copy)
              (define-key org-mode-map (kbd "C-c j") #'bima/next-subtree-narrow)
              (define-key org-mode-map (kbd "C-c k") #'bima/prev-subtree-narrow))

            ;; (evil-set-initial-state 'image-mode 'emacs)
            ;; (define-key image-mode-map (kbd "q") 'kill-this-buffer)

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
            (global-set-key (kbd "C-c i") 'org-capture-inbox)
            (global-set-key (kbd "C-c l") 'org-store-link)

            ;; (global-set-key (kbd "<f3>") 'neotree-toggle)
            (global-set-key (kbd "<f3>") 'dirvish-side)
            (global-set-key (kbd "C-x g") 'magit-status)


            (custom-set-faces
             '(evil-mc-region-face ((t (:inherit cursor)))))))
