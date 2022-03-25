;; -*- lexical-binding: t; -*-
(setq user-full-name "Bima Wiratama")
(setq user-mail-address "bimakope@gmail.com")

(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

;; From doom emacs FAQ
(defun doom-defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun doom-restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook #'doom-defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'doom-restore-garbage-collection-h)

(defun bima/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
	                 (float-time
	                  (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 60000000 ; 16mb
                  gc-cons-percentage 0.1)))

(add-hook 'emacs-startup-hook #'bima/display-startup-time)

(add-hook 'after-init-hook (lambda ()
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install use package and update repositories if use-package not installed
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)))


;; ------------------------------------------------------------
;; UI Tweaks
;; ------------------------------------------------------------
(setq package-native-compile t)
(setq package-quickstart t) ; Newest Change
(setq package-enable-at-startup nil)
;;(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq site-run-file nil)
(setq byte-compile-warnings '(cl-functions)) ; remove cl warning
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq initial-frame-alist (quote ((fullscreen . maximized))))
(add-hook 'after-init-hook 'save-place-mode)
;; (setq initial-major-mode 'fundamental-mode)
(setq-default tab-width 4)
(setq-default line-spacing 3)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)
(menu-bar-mode -1)
(tool-bar-mode 0) ;;; Remove Toolbar
(scroll-bar-mode 0) ;;; Remove Scrollbar
(blink-cursor-mode 1) ;;; Remove Blink Cursor
(global-visual-line-mode t)

(global-prettify-symbols-mode +1)
(setq display-line-numbers-type 'relative)
(set-face-attribute 'line-number-current-line nil :weight 'bold)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq cursor-type 'box)

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)

(setq window-divider-default-right-width 1)
(setq window-divider-default-places 'right-only)
(window-divider-mode)

(defvar jikope/default-font-size 105)
(set-face-attribute 'default 'nil :family "Iosevka Term" :weight 'light :height jikope/default-font-size)
(set-face-attribute 'fixed-pitch 'nil :family "Iosevka Term" :height 105 :weight 'light)
(set-face-attribute 'variable-pitch nil :font "Roboto Condensed" :height 125 :weight 'light)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-bar-width 2
        doom-modeline-icon nil
        doom-modeline-mu4e t
        doom-modeline-modal-icon nil
        doom-modeline-height 20
        doom-modeline-buffer-file-name-style 'relative-from-project)
  (mu4e-alert-enable-mode-line-display)
  (set-face-attribute 'mode-line nil :height 85)
  (set-face-attribute 'mode-line-inactive nil :height 80))

(use-package mlscroll
  :ensure t
  :after doom-modeline
  ;;:hook (after-init . mlscroll-mode)
  :config
  (require 'which-func)
  (setq mlscroll-shortfun-min-width 11)
  (mlscroll-mode))

(defun load-wsl-theme ()
  (add-to-list 'custom-theme-load-path '"~/Git-Application/wsl-emacs/themes/")
  (load-theme 'microsoft-teams-dark t))

(add-hook 'after-init-hook 'load-wsl-theme)
;;(add-to-list 'custom-theme-load-path '"~/Git-Application/wsl-emacs/themes/")

(use-package all-the-icons
  :ensure t
  :defer t)

(use-package wsd-mode
  :ensure t
  :defer t)


(with-eval-after-load 'org
  ;; ------------------------------------------------------------
  ;; ORG UI
  ;; ------------------------------------------------------------
  (setq header-line-format " ")
  (setq org-startup-indented t
        org-image-actual-width nil
        org-startup-folded t
        org-pretty-entities t 
        org-hide-emphasis-markers t ; Hide italic, bold ,etc
        org-agenda-block-separator ""
        org-ellipsis " ▾"
        org-src-preserve-indentation t)

  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :weight 'regular :height (cdr face)))
  (set-face-attribute 'org-todo 'nil :weight 'bold :box 'nil)

  (add-hook 'org-mode-hook 'visual-line-mode)

  (defun bima/org-set-font ()
    (variable-pitch-mode)
    (org-modern-mode))

  (add-hook 'org-mode-hook #'bima/org-set-font)

  (defun bima/next-subtree-narrow ()
    "Move screen to next subtree and narrow it"
    (interactive)
    (widen)
    (org-forward-heading-same-level 1)
    (org-narrow-to-subtree))

  (defun bima/prev-subtree-narrow ()
    "Move screen to previous subtree and narrow it"
    (interactive)
    (widen)
    (org-backward-heading-same-level 1)
    (org-narrow-to-subtree))

  ;; ------------------------------------------------------------
  ;; ORG TIMER
  ;; ------------------------------------------------------------
  ;;(setq org-clock-sound "~/MVI_0266.mp4")

  ;; ------------------------------------------------------------
  ;; ORG KEYBINDINGS
  ;; ------------------------------------------------------------
  (define-key org-mode-map (kbd "C-S-<left>") 'shrink-window-horizontally)
  (define-key org-mode-map (kbd "C-S-<right>") 'enlarge-window-horizontally)
  (define-key org-mode-map (kbd "C-S-<down>") 'shrink-window)
  (define-key org-mode-map (kbd "C-S-<up>") 'enlarge-window)
  (define-key org-mode-map (kbd "C-j") nil)
  (define-key org-mode-map (kbd "<tab>") 'org-cycle)
  (define-key org-mode-map (kbd "C-j") 'windmove-down)
  (define-key org-mode-map (kbd "C-k") nil)
  (define-key org-mode-map (kbd "C-k") 'windmove-up)
  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link-hydra)    ;(define-key org-agenda-mode-map (kbd "C-k") nil)
  (add-hook 'org-agenda-mode-hook (lambda () (local-set-key (kbd "C-k") 'windmove-down)))
  (define-key org-mode-map (kbd "C-S-n") 'org-forward-heading-same-level)
  (define-key org-mode-map (kbd "C-S-p") 'org-backward-heading-same-level)

  ;; ------------------------------------------------------------
  ;; ORG BABEL
  ;; ------------------------------------------------------------
  (defun my-org-confirm-babel-evaluate (lang)
    (not (member lang '("sh" "python" "sql" "js" "latex" "emacs-lisp"))))

  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

  ;; ob-js FIX
  (defvar org-babel-js-function-wrapper "console.log(JSON.stringify(require('util').inspect(function(){\n%s\n}())));")

  (use-package ob-ipython
    :ensure t)

  (use-package ob-restclient
    :ensure t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (restclient . t) (shell . t) (sql . t) (js . t) (latex . t)))

  (add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))

  ;; ------------------------------------------------------------
  ;; AGENDA RELATED
  ;; ------------------------------------------------------------

  (use-package org-super-agenda
    :after org-agenda
    :ensure t
    :hook (org-agenda-mode . org-super-agenda-mode))

  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '((:name "Dinten Puniki"
                                  :time-grid t
                                  :todo "TODAY")
                           (:discard (:anything))))))
            (todo "" ((org-agenda-overriding-header "")
                      (org-super-agenda-groups
                       '((:name "Projects"
                                :file-path "projects"
                                :and (:todo "TODO" :tag "ACTIVE" :tag "Coding")
                                :discard (:todo "PROJECT"))
                         (:name "Overdue"
                                :scheduled past
                                :deadline past)
                         (:discard (:anything))))))))))

  (setq org-log-done 'time)
  (setq org-log-reschedule 'time)
  (setq org-log-into-drawer t)
  (setq org-directory "~/Roam")
  (setq org-agenda-skip-scheduled-if-done nil
        org-agenda-skip-deadline-if-done t
        org-agenda-time-leading-zero nil
        org-agenda-compact-blocks t
        org-agenda-include-deadlines t
        org-agenda-remove-tags t
        org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now"
        org-agenda-scheduled-leaders '("" "")

        org-agenda-time-grid (quote ((today require-timed remove-match) () "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈"))
        org-agenda-prefix-format "   %i %?-8:c %?-2t% s")

  (setq org-agenda-category-icon-alist
        `(
          ("Book" "~/dotfiles/icons/book-open-page-variant-outline.svg" nil nil :ascent center :mask heuristic)
          ("Class" "~/dotfiles/icons/clipboard-text-multiple-outline.svg" nil nil :ascent center :mask heuristic)
          ("Coding" "~/dotfiles/icons/code-json.svg" nil nil :ascent center :mask heuristic)
          ("Config" "~/dotfiles/icons/cog-outline.svg" nil nil :ascent center :mask heuristic)
          ("Bill" "~/dotfiles/icons/cash-multiple.svg" nil nil :ascent center :mask heuristic)
          ("" "~/dotfiles/icons/calendar-check-outline.svg" nil nil :ascent center :mask heuristic)
          ))

  (setq org-agenda-files '("~/agenda/life.org" "~/agenda/manga.org" "~/agenda/projects.org"))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROJECT(p)" "WAITING(w)" "MAYBE(m)" "|" "DONE(d)" "COMPLETED(c)" "CANCELLED(l)")))
  (setq org-tag-alist '(
                        ("Learn". ?l) ("ACTIVE" . ?a) ("Project".?p) ("Other") ("Daily") ("Book". ?b) ("Class" . ?c)))
  (add-hook 'org-agenda-mode-hook #'evil-mode)

  ;; ------------------------------------------------------------
  ;; LATEX OPTIONS
  ;; ------------------------------------------------------------
  (add-to-list 'org-latex-packages-alist '("" "minted" "xcolor"))
  (setq org-latex-default-packages-alist ; Remove hyperref in latex preview
        '(("AUTO" "inputenc" t
           ("pdflatex"))
          ("T1" "fontenc" t
           ("pdflatex"))
          ("" "graphicx" t nil)
          ("" "longtable" nil nil)
          ("" "wrapfig" nil nil)
          ("" "rotating" nil nil)
          ("normalem" "ulem" t nil)
          ("" "amsmath" t nil)
          ("" "amssymb" t nil)
          ("" "capt-of" nil nil)
          ("colorlink=true,linkcolor=black,citecolor=blue" "hyperref" t nil)))

  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process '(
			                          "latexmk -shell-escape -interaction=nonstopmode -bibtex  -pdflatex=%latex -pdf -quiet -f %f"
			                          "latexmk -shell-escape -interaction=nonstopmode -bibtex  -pdflatex=%latex -pdf -quiet -f %f"
			                          ;; "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
			                          ;; "bibtex %b"
			                          ;; "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
			                          ))

  ;; Sample minted options.
  (setq org-latex-minted-options '(
				                           ("frame" "single")
				                           ("breaklines" "true")
				                           ("breakanywhere" "true")
				                           ("xleftmargin" "\\parindent")
				                           ))

  (setq org-refile-targets
        '((org-agenda-files :maxlevel . 5)))

  ;;(setq org-refile-use-outline-path 'file)
  (setq org-refile-use-outline-path t)
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  ) ;; END OF ORG MODE HOOK

(use-package org-roam
  :ensure t
  :defer t
  :custom
  (org-roam-directory (file-truename "~/Roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n t" . org-roam-tag-add)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+date: %u\n")
           :unnarrowed t)))

  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-roam-bibtex
  :ensure t
  :after (org-roam helm-bibtex)
  :bind (:map org-mode-map ("C-c n b" . orb-note-actions))
  :config
  (require 'org-ref)
  (org-roam-bibtex-mode))

(setq org-capture-templates '(
                              ("t" "Task" entry
                               (file+headline "~/agenda/life.org" "Inbox")
                               "* TODO %?\n:PROPERTIES:\n:CREATED:%U\n:END:\n\n"
                               :empty-lines 1)
                              ("m" "Manga" entry
                               (file+headline "~/agenda/Manga.org" "Inbox")
                               "* READ %?")
                              ))

(use-package org-download
  :ensure t
  :after org)

(use-package org-ref
  :ensure t
  :after org
  :config
  (setq biblio-bibtex-use-autokey t
        bibtex-dialect 'biblatex
        bibtex-completion-bibliography '("~/Uni/bib/references.bib")
        bibtex-completion-library-path "~/Uni/bib/pdf/"
        bibtex-completion-notes-path "~/Uni/bib/notes/"
        bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-display-formats
        '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
	      bibtex-completion-pdf-open-function
	      (lambda (fpath)
          (call-process "open" nil 0 nil fpath)))

  (require 'bibtex)

  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5
        org-ref-bibtex-hydra-key-binding (kbd "C-c b"))

  (setq bib-files-directory (directory-files
                             (concat (getenv "HOME") "/Uni/bib") t
                             "^[A-Z|a-z].+.bib$")
        pdf-files-directory (concat (getenv "HOME") "/Uni/bib/pdf"))

  (setq bibtex-user-optional-fields '(("keywords" "Keywords to describe the entry" "")
                                      ("file" "Link to document file." ":"))
        bibtex-align-at-equal-sign t)

  (define-key bibtex-mode-map (kbd "C-c b") 'org-ref-bibtex-hydra/body))

(use-package helm-bibtex
  :ensure t
  :after org-roam
  :config
  (setq bibtex-completion-bibliography bib-files-directory
        bibtex-completion-library-path pdf-files-directory
        bibtex-completion-pdf-field "File"
        bibtex-completion-notes-path org-directory
        bibtex-completion-additional-search-fields '(keywords))
  :bind
  (("C-c n B" . helm-bibtex)))

(use-package org-noter
  :ensure t
  :after org-roam
  :config
  (setq org-noter-notes-search-path "~/Uni/bib/pdf"))

;; (use-package pdf-continuous-scroll-mode
;;   :load-path "~/.emacs.d/plugins/pdf-continuous-scroll-mode.el"
;;   :after pdf-tools)

(use-package org-pdftools
  :ensure t
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :ensure t
  :after org-noter
  :config
  (setq  org-noter-pdftools-use-org-id nil ; Remove ID on PROPERTIES drawer 
         org-noter-pdftools-use-unique-org-id nil) 
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))


;; ------------------------------------------------------------
;; PDF TOOLS & LATEX 
;; ------------------------------------------------------------
(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page))
  :config
  (pdf-tools-install :no-query))


;; ------------------------------------------------------------
;; PROJECT MANAGEMENT 
;; ------------------------------------------------------------
(use-package projectile
  :ensure t
  :hook (emacs-startup . projectile-mode)
  :config 
  (setq projectile-project-search-path '("/media/data/Web-Applications"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package ripgrep
  :ensure t
  :after projectile)

(use-package magit
  :ensure t
  :after projectile)

(use-package magit-todos
  :ensure t
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
    (setq buffer-face-mode-face '(:family "Roboto" :height 100))
    (buffer-face-mode)
    (visual-line-mode -1))
  (add-hook 'neotree-mode-hook #'neotree-set-font)
  (define-key neotree-mode-map (kbd "<tab>") 'neotree-enter)
  (define-key neotree-mode-map (kbd "C-c t h") 'neotree-hidden-file-toggle))


;; ------------------------------------------------------------
;; EVIL MODE 
;; ------------------------------------------------------------
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


;; ------------------------------------------------------------
;; COMPLETION SYSTEM 
;; ------------------------------------------------------------
(use-package vertico
  :ensure t
  :hook (emacs-startup . vertico-mode)
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

  (setq read-extended-command-predicate #'command-completion-default-include-p)
  (setq enable-recursive-minibuffers t))

(use-package marginalia
  :ensure t
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
;; EMAIL CLIENT 
;; ------------------------------------------------------------
(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type  'ssl) 

;; Required package isync
;; Configuration file ~/.mbsyncrc
(use-package mu4e
  :ensure nil
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :defer 15 ; Wait until 20 seconds after startup
  :config
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 60 60)
        mu4e-get-mail-command "mbsync -a"
        mu4e-maildir "~/Mail"
        mu4e-compose-format-flowed t
        mu4e-compose-context-policy 'ask-if-none
        message-send-mail-function 'smtpmail-send-it)

  (setq mu4e-contexts
        (list
         ;; Primary Gmail Account
         (make-mu4e-context
          :name "Main"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "bimakope@gmail.com")
                  (user-full-name    . "Bimakope")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/Gmail[Gmail]/Drafts")
                  (mu4e-sent-folder  . "/Gmail[Gmail]/Sent Mail")
                  (mu4e-refile-folder  . "/Gmail[Gmail]/All Mail")
                  (mu4e-trash-folder  . "/Gmail[Gmail]/Trash")))
         ;; University Account
         (make-mu4e-context
          :name "Uni"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Uni" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "bima.3022@students.amikom.ac.id")
                  (user-full-name    . "Bima Wiratama")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/Uni[Gmail]/Drafts")
                  (mu4e-sent-folder  . "/Uni[Gmail]/Sent Mail")
                  (mu4e-refile-folder  . "/Uni[Gmail]/All Mail")
                  (mu4e-trash-folder  . "/Uni[Gmail]/Trash")))))

  (setq mu4e-maildir-shortcuts
        '(("/Gmail/Inbox"             . ?i)
          ("/Gmail/[Gmail]/Sent Mail" . ?s)
          ("/Gmail/[Gmail]/Trash"     . ?t)
          ("/Gmail/[Gmail]/Drafts"    . ?d)
          ("/Gmail/[Gmail]/All Mail"  . ?a))))

(use-package mu4e-alert
  :ensure t
  :hook (mu4e-mode . mu4e-alert-enable-mode-line-display)
  :config
  ;;(mu4e-alert-enable-mode-line-display)
  (mu4e-alert-set-default-style 'libnotify)
  (setq mu4e-alert-notify-repeated-mails t)
  (setq mu4e-alert-enable-notifications t))


;; ------------------------------------------------------------
;; EMACS UTILITIES 
;; ------------------------------------------------------------
(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  (define-key which-key-mode-map (kbd "<escape>") 'keyboard-escape-quit))

;; too slow to be used
;; (use-package helpful 
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "<f1> f") #'helpful-callable)

;;   (global-set-key (kbd "<f1> v") #'helpful-variable)

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 1)
  (setq company-backends
        '((company-files          ; files & directory
           company-keywords       ; keywords
           company-capf)  ; completion-at-point-functions
          (company-abbrev company-dabbrev)
          )))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "SPC") #'company-abort))

(use-package company-box
  :ensure t
  :after company
  :hook (company-mode . company-box-mode))

(use-package writeroom-mode
  :ensure t
  :after org
  :config
  (setq writeroom-fullscreen-effect 'maximized)
  (setq writeroom-width 120)
  (setq writeroom-mode-line 't))


;; ------------------------------------------------------------
;; LSP MODE
;; ------------------------------------------------------------
(use-package lsp-mode
  :ensure t
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

;; ------------------------------------------------------------
;; PROG MODE
;; ------------------------------------------------------------
(use-package flycheck
  :ensure t
  :defer t
  :hook (prog-mode . flycheck-mode))

;; (use-package line-reminder
;;   :ensure t
;;   :hook (prog-mode . line-reminder-mode))

(use-package aggressive-indent
  :ensure t
  :hook (org-mode . aggressive-indent-mode)
  :config
  (add-hook 'prog-mode-hook (lambda ()
                              (unless (eq major-mode 'c++-mode)
                                (aggressive-indent-mode)))))

(use-package editorconfig
  :ensure t
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

(font-lock-add-keywords 'vue-mode
                        `((,(concat
			                       "\\<[_a-zA-Z][_a-zA-Z0-9]*\\>"       ; Object identifier
			                       "\\s *"                              ; Optional white space
			                       "\\(?:\\.\\|->\\)"                   ; Member access
			                       "\\s *"                              ; Optional white space
			                       "\\<\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\>" ; Member identifier
			                       "\\s *"                              ; Optional white space
			                       "(")                                 ; Paren for method invocation
			                     1 'font-lock-function-name-face t)))

(defface font-lock-method-call-face
  '((t . (:foreground "#000000" :bold t)))
  "Face to display method calls in.")

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq tab-width 2)))

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
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package highlight-indent-guides
  :ensure t
  :defer t
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
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package evil-mc
  :ensure t
  :hook
  (prog-mode . evil-mc-mode)
  :config
  (define-key evil-normal-state-map (kbd "C-c <return>") 'evil-mc-undo-all-cursors)
  (global-set-key (kbd "C-S-<mouse-1>") 'evil-mc-toggle-cursor-on-click))

(use-package pyvenv
  :ensure t
  :defer t
  :config
  (setq pyvenv-menu t)
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
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

;; OMNISHARP
(use-package csharp-mode
  :ensure t
  :after projectile)

(use-package omnisharp
  :ensure t
  :hook (csharp-mode . omnisharp-mode))

(use-package modern-cpp-font-lock
  :ensure t
  :hook (c++-mode . modern-c++-font-lock-mode))

(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))

;; ------------------------------------------------------------
;; WEB DEVELOPMENT
;; ------------------------------------------------------------
(use-package restclient
  :ensure t
  :defer t)

(use-package emmet-mode
  :ensure t
  :hook (sgml-mode . emmet-mode)
  :config 
  (define-key emmet-mode-keymap (kbd "C-j") nil)
  (define-key evil-insert-state-map (kbd "C-e") 'emmet-expand-line))

(use-package add-node-modules-path
  :ensure t
  :defer t)

(use-package typescript-mode
  :ensure t
  :defer t
  :mode
  ("\\.ts\\'" . typescript-mode)
  :hook
  (typescript-mode . lsp-deferred)
  :commands (typescript-mode)
  :config
  (setq typescript-indent-level 4))
;; :init
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
;; (setq typescript-indent-level 4))

(use-package prettier-js
  :ensure t
  :hook (rjsx-mode . #'prettier-js-mode)
  :config
  (setq prettier-js-args '("--print-width" "120"
			                     "--tab-width" "2")))

(use-package js2-mode
  :ensure t
  :defer t
  :mode
  ("\\.js\\'" . js2-mode)
  :hook
  (js2-mode . lsp-deferred))


(use-package rjsx-mode
  :ensure t 
  :mode ("\\.jsx\\'")
  :hook
  (rjsx-mode . lsp-deferred)
  :config
                                        ;(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
  (add-hook 'rjsx-mode-hook #'prettier-js-mode)
  (add-hook 'rjsx-mode-hook #'flycheck-mode)

  (eval-after-load 'js2-mode
    '(progn
       (add-hook 'js2-mode-hook #'add-node-modules-path)
       )))

(use-package vue-mode
  :ensure t
  :after projectile
  :config
  (defun html-backend-hook ()
    "Hook for `web-mode'."
    (set (make-local-variable 'company-backends)
         '(company-web-html company-yasnippet company-files)))

  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  (add-hook 'vue-mode-hook 'html-backend-hook)
  (add-hook 'vue-mode-hook #'lsp-deferred)
  (add-hook 'vue-mode-hook 'display-line-numbers-mode)
                                        ;(add-hook 'vue-mode-hook 'prettier-js-mode)
                                        ;(setq prettier-js-args '("--parser vue"))

  (eval-after-load 'vue-mode
    '(progn
       (add-hook 'vue-mode-hook #'add-node-modules-path)
       )
    ))

(use-package php-mode
  :ensure t
  :mode "\\.php\\'"
  :hook (php-mode . lsp-deferred))

(use-package web-mode
  :ensure t
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
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-indent-style 4))


;; ------------------------------------------------------------
;; VTERM 
;; ------------------------------------------------------------
(use-package vterm
  :ensure t
  :after projectile
  :config
  ;;(setq vterm-keymap-exceptions (quote "C-j" "C-k" "C-l"))
  (define-key vterm-mode-map (kbd "C-l") nil)
  (define-key vterm-mode-map (kbd "C-j") nil)
  (define-key vterm-mode-map (kbd "C-h") nil)
  (define-key vterm-mode-map (kbd "C-k") nil)
  (define-key vterm-mode-map (kbd "<f5") nil)

  (define-key vterm-mode-map (kbd "C-l") #'windmove-right)
  (define-key vterm-mode-map (kbd "C-j") #'windmove-down)
  (define-key vterm-mode-map (kbd "C-h") #'windmove-left)
  (define-key vterm-mode-map (kbd "C-k") #'windmove-up)
  (define-key vterm-mode-map (kbd "<f5>") #'kill-this-buffer)

  (define-key vterm-mode-map (kbd "C-S-h") 'shrink-window-horizontally)
  (define-key vterm-mode-map (kbd "C-S-l") 'enlarge-window-horizontally)
  (define-key vterm-mode-map (kbd "C-S-j") 'shrink-window)
  (define-key vterm-mode-map (kbd "C-S-k") 'enlarge-window)

  (defun bima/open-split-term ()
    "Open Terminal and split Vertically"
    (interactive)
    (split-window-below)
    (windmove-down)
    (vterm "*Terminal*")
    (setq mode-line-format nil)
    (evil-emacs-state)
    )
  (global-set-key (kbd "<s-return>") 'bima/open-split-term))


;; ------------------------------------------------------------
;; CUSTOM KEYBINDINGS 
;; ------------------------------------------------------------
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
             '(evil=mc=region-face ((t (:inherit cursor)))))))

