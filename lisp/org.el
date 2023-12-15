;; ------------------------------------------------------------
;; ORG UI
;; ------------------------------------------------------------
(setq org-startup-indented t
      org-image-actual-width '(500)
      org-startup-folded t
      org-pretty-entities t 
      org-hide-emphasis-markers nil ; Hide italic, bold ,etc
      org-agenda-block-separator ""
      org-ellipsis " ▾"
      org-src-preserve-indentation t)

(setq org-export-headline-levels 6
      org-list-allow-alphabetical 't)
;; (use-package org-modern
;;   :straight t)

(use-package org-modern
  :straight (org-modern :type git :host github :repo "minad/org-modern"
                        :fork (:host github
                                     :repo "BlankSpruce/org-modern")))

(setq org-modern-todo nil
      org-modern-tag nil
      org-modern-timestamp nil
      org-modern-statistics nil
      org-modern-checkbox nil
      org-modern-table nil
      org-modern-hide-stars 'leading
      ;; org-modern-keyword nil
      org-modern-priority nil)

(dolist (face '((org-level-1 . 1.25)
                (org-level-2 . 1.15)
                (org-level-3 . 1.1)
                (org-level-4 . 1.1)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :weight 'regular :height (cdr face)))

(set-face-attribute 'org-todo 'nil :weight 'light :box 'nil :foreground "#ff6f00")

(set-face-attribute 'org-code 'nil :inherit 'fixed-pitch)
(set-face-attribute 'org-block 'nil :inherit 'fixed-pitch)
(set-face-attribute 'org-ellipsis 'nil :underline nil)
(set-face-attribute 'org-date 'nil :inherit 'org-macro :underline nil)
;; (set-face-attribute 'org-table 'nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'modus-themes-fg-magenta-cooler :foreground 'nil :weight 'regular)
(set-face-attribute 'org-modern-done 'nil :background nil)
(set-face-attribute 'org-modern-todo 'nil :background nil :foreground nil)
(set-face-attribute 'org-checkbox-statistics-done 'nil :background nil)

(add-hook 'org-mode-hook 'visual-line-mode)
;;(add-hook 'org-mode-hook #'org-modern-mode)
;; (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
;(add-hook 'org-agenda-finalize-hook #'org-agenda-log-mode)

;; SVG tags, progress bars & icons
(use-package svg-lib
  :straight t)
;;  ;'(svg-lib :type git :host github :repo "rougier/svg-lib"))

;; ;; Replace keywords with SVG tags
(use-package svg-tag-mode
  :straight (svg-tag-mode :type git :host github :repo "rougier/svg-tag-mode"))
 ;'(svg-tag-mode :type git :host github :repo "rougier/svg-tag-mode"))

(defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
(defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
(defconst day-re "[A-Za-z]\\{3\\}")
(defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

(defun svg-progress-percent (value)
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                    nil :margin 0 :stroke 2 )
              (svg-lib-tag (concat value "%")
                           nil :stroke 0 :margin 0 :font-size jikope/svg-tag-size :height jikope/svg-tag-height)) :ascent 'center))

(defun svg-progress-count (value)
  (let* ((seq (mapcar #'string-to-number (split-string value "/")))
         (count (float (car seq)))
         (total (float (cadr seq))))
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ count total) nil
                                    :margin 0 :stroke 2 )
              (svg-lib-tag value nil
                           :font-size 11 :stroke 0 :margin 0)) :ascent 'center)))

(setq svg-tag-tags
      `(
        ;; Org tags
        ;; (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-level-1))))
        ;; (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))
        ("\\(:#[A-Za-z0-9]+\\)" . ((lambda (tag)
                                     (svg-tag-make tag :beg 2))))
        ("\\(:#[A-Za-z0-9]+:\\)$" . ((lambda (tag)
                                       (svg-tag-make tag :beg 2 :end -1))))
        
        ;; Task priority
        ("\\[#[A-Z]\\]" . ( (lambda (tag)
                              (svg-tag-make tag :face 'org-priority 
                                            :beg 2 :end -1 :margin 0))))

        ;; ;; Progress
        ;; ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
        ;;                                     (svg-progress-percent (substring tag 1 -2)))))
        ;; ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
        ;;                                   (svg-progress-count (substring tag 1 -1)))))
        
        ;; TODO / DONE
        ("* \\(TODO\\)" . ((lambda (tag) (svg-tag-make "TODO" :face 'error :inverse nil :margin 0 :height jikope/svg-tag-height))))
        ("* \\(DONE\\)" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0 :height jikope/svg-tag-height))))

        ;; ORG options
        ("#+TITLE:" . ((lambda (tag) (svg-tag-make "ASD" :face dired-rainbow-shell-face :height jikope/svg-tag-height))))

        ;; Citation of the form [cite:@Knuth:1984] 
        ("\\(\\[cite:&[A-Za-z]+:\\)" . ((lambda (tag)
                                          (svg-tag-make tag
                                                        :inverse t
                                                        :beg 7 :end -1
                                                        :crop-right t))))
        ("\\[cite:&[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                   (svg-tag-make tag
                                                                 :end -1
                                                                 :crop-left t
                                                                 :height jikope/svg-tag-height))))

        ;; [C-x] [C-f]
        ("||[0-9a-zA-Z- ]+?||" . ((lambda (tag)
                                  (svg-tag-make tag :face 'font-lock-comment-face
                                                :margin 0 :beg 2 :end -2 :height jikope/svg-tag-height))))
        
        ;; Active date (with or without day name, with or without time)
        (,(format "\\(<%s>\\)" date-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date :height jikope/svg-tag-height))))
        (,(format "\\(<%s \\)%s>" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date :height jikope/svg-tag-height))))
        (,(format "<%s \\(%s>\\)" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date :height jikope/svg-tag-height))))

        ;; Inactive date  (with or without day name, with or without time)
        (,(format "\\(\\[%s\\]\\)" date-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date :height jikope/svg-tag-height))))
        (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date :height jikope/svg-tag-height))))
        (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date :height jikope/svg-tag-height))))))

(use-package org-modern-indent
  :straight nil
  :load-path "~/.emacs.d/plugins/org-modern-indent")

(use-package imenu-list
  :straight t
  :custom (imenu-list-position 'left))

(use-package exiftool
  :straight t)

(use-package org-imenu
  :straight nil
  :after org
  :load-path "~/.emacs.d/plugins/org-imenu"
  :config
  (setq org-imenu-depth 2))

;; (use-package sideframe
;;   :load-path "~/.emacs.d/plugins/sideframe")

;; (use-package pdf-drop-mode
;;   :load-path "~/.emacs.d/plugins/pdf-drop-mode")

;; Rougier Org bib mode 
;; https://github.com/rougier/org-bib-mode
(use-package org-imenu
 :straight (org-imenu :type git :host github :repo "rougier/org-imenu"))

;; (use-package book-mode
;;   :straight nil
;;   :load-path "~/.emacs.d/plugins/book-mode"
;;   :custom
;;   book-mode-top-margin 0
;;   book-mode-bottom-margin 0
;;   book-mode-left-margin 8)

(use-package pdf-drop-mode
 :straight (pdf-drop-mode :type git :host github :repo "rougier/pdf-drop-mode"))

;(use-package citeproc
;  :straight t
;  :after org)

(use-package org-bib
  :straight (org-bib :type git :host github :repo "rougier/org-bib-mode"))
  ;:load-path "~/.emacs.d/plugins/org-bib-mode"
  ;; :custom
  ;; (org-bib-library-path "/home/jikope/Uni/Riset/")
  ;; (org-bib-library-file "/home/jikope/Uni/Riset/papers.org"))

(defun jikope/toggle-transparent ()
  (interactive)
  (if (get 'jikope/transparent-state 'state)
      (progn
	    (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
	    (add-to-list 'default-frame-alist '(alpha . '(100 . 100)))
	    (put 'jikope/transparent-state 'state nil))
    (progn
      (set-frame-parameter (selected-frame) 'alpha '(85 . 100))
      (add-to-list 'default-frame-alist '(alpha . '(90 . 100)))
      (put 'jikope/transparent-state 'state t))))


(defun bima/org-left-margin()
  (add-to-list 'font-lock-extra-managed-props 'display)
  (setq left-margin 5)
  (setq left-margin-width left-margin)
  (set-window-margins (selected-window) left-margin 0)
  (let ((margin-format (format "%%%ds" left-margin-width)))
    (font-lock-add-keywords nil
       `(
         ("^#\\+begin_abstract.*?\\(\n\\)"
          1 '(face nil display " "))
         
         ("^\\(#\\+begin_abstract.*$\\)"
          1 '(face nano-default display (,(concat
          (propertize "Abstract."
                   'face '(:inherit nano-strong)))
               append)))
         
         ("\\(\n#\\+end_abstract.*\\)$"
          1 '(face nano-default display (,(concat
          (propertize " —"
                   'face '(:inherit nano-strong)))
          append)))

         ("^#\\+begin_keywords.*?\\(\n\\)"
          1 '(face nil display " "))
         
         ("^\\(#\\+begin_keywords.*$\\)"
          1 '(face nano-default display (,(concat
          (propertize "Keywords:"
                   'face '(:inherit nano-strong)))
               append)))
         
         ("\\(\n#\\+end_keywords.*\\)$"
          1 '(face nano-default display (,(concat
          (propertize " —"
                   'face '(:inherit nano-strong)))
          append)))

         ("^\\(\\- \\)\\(.*\\)$"
          1 '(face nano-default display ((margin left-margin)
                                         ,(propertize (format margin-format "• ")
                                                      'face '(:inherit nano-default :weight light)) append)))

         ("^\\(\\*\\{1\\} \\)\\(.*\\)$"
          1 '(face nano-faded display ((margin left-margin)
                                       ,(propertize (format margin-format "# ")
                                                    'face '(:inherit nano-faded :weight light)) append))
          2 '(face bold append))

         ("^\\(\\*\\{2\\} \\)\\(.*\\)$"
          1 '(face nano-faded display ((margin left-margin)
                                       ,(propertize (format margin-format "## ")
                                                    'face '(:inherit nano-faded :weight light)) append))
          2 '(face bold append))

         ("^\\(\\*\\{3\\} \\)\\(.*\\)$"
          1 '(face nano-faded display ((margin left-margin)
                                       ,(propertize (format margin-format "### ")
                                                    'face '(:inherit nano-faded :weight light)) append))
          2 '(face bold append))

         ("^\\*\\{4\\} .*?\\(\n\\)"
          1 '(face nil display " - "))

         ("^\\(\\*\\{4\\} \\)\\(.*?\\)$"
          1 '(face nano-faded display ((margin left-margin)
                                       ,(propertize (format margin-format "§ ")
                                                    'face '(:inherit nano-faded :weight light))  append))
          2 '(face bold append)))))

  (font-lock-fontify-buffer)
  ;; (visual-line-mode)
  )

(defun bima/setup-writing-environment ()
  "Setup a nice writing environment."
  (interactive)
  (setq-local line-spacing 5)
  (setq org-modern-hide-stars 't
        org-imenu-depth 3)
  (custom-set-faces '(org-modern-label ((t (:width normal)))))
  ;; (setq-local header-line-format " ")
  ;; (custom-set-faces '(header-line ((t (:height 200 :foreground nil :background nil)))))
  (org-mode)
  (org-imenu)
  (variable-pitch-mode)
  (bima/org-left-margin)
  (org-indent-mode -1)
  ;; (book-mode)
  (org-modern-mode)
  (setq org-modern-hide-stars 'leading
        org-imenu-depth 2)
  (svg-tag-mode)) 

(defun bima/default-org-hook ()
  "Default hook for org-mode"
  (setq-local line-spacing 0)
  ;; (custom-set-faces '(org-modern-label ((t (:width normal)))))
  (variable-pitch-mode)
  ;; (org-indent-mode -1)
  (org-modern-mode)
  (svg-tag-mode))

(add-hook 'org-mode-hook 'bima/default-org-hook)
;; (add-hook 'org-mode-hook #'org-modern-indent-mode 90)
(set-face-attribute 'fixed-pitch nil :height 1.0)

(defun bima/next-subtree-narrow ()
  "Move screen to next subtree and narrow it."
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
  (not (member lang '("sh" "python" "sqlite" "sql" "js" "latex"))))

(setq org-confirm-babel-evaluate 'nil)

;; ob-js FIX
(defvar org-babel-js-function-wrapper "console.log(JSON.stringify(require('util').inspect(function(){\n%s\n}())));")

;; (use-package ob-ipython
;;   :straight t)

(use-package ob-restclient
  :straight t)

(use-package ob-mongo
  :straight t)

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((python . t) (restclient . t) (shell . t) (sql . t) (js . t) (latex . t) (mongo . t)))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (js . t) (latex . t) (sqlite . t)))


(add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))

;; ------------------------------------------------------------
;; AGENDA RELATED
;; ------------------------------------------------------------

(use-package org-super-agenda
  :after org-agenda
  :straight t
  :hook (org-agenda-mode . org-super-agenda-mode))

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-overriding-header "")
                      (org-super-agenda-groups
                       '((:name "Dinten Puniki"
                                :time-grid t
                                :todo "TODAY"
                                :todo "DONE")
                         (:name "Today but free hour"
                                :and (:scheduled today :not (:time-grid t)))
                         (:name "Done Today"
                                :todo "DONE")
                         (:discard (:anything))))))
          (todo "" ((org-agenda-overriding-header "")
                    (org-super-agenda-groups
                     '(
                       ;; (:name "Projects"
                       ;;        :file-path "projects"
                       ;;        :and (:todo "TODO" :tag "ACTIVE" :tag "Coding")
                       ;;        :discard (:todo "PROJECT" :habit ))
                       (:name "Overdue"
                              :scheduled past
                              :deadline past)
                       (:discard (:anything))))))))))

(setq org-log-done 'time)
(setq org-log-reschedule 'time)
(setq org-log-into-drawer t)
(setq org-directory "~/Roam")
(setq org-agenda-start-with-log-mode t
      org-agenda-skip-scheduled-if-done nil
      org-agenda-skip-deadline-if-done t
      org-agenda-time-leading-zero nil
      org-agenda-compact-blocks t
      org-agenda-include-deadlines t
      org-agenda-remove-tags t
      org-agenda-current-time-string "ᐊ┈┈┈┈┈┈┈ Now"
      org-agenda-scheduled-leaders '("" "")

      org-agenda-time-grid (quote ((today require-timed remove-match) () "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈"))
      org-agenda-prefix-format "   %i %?-8:c    %?-2t% s")

;; ORG HABIT
(setq org-habit-today-glyph ?◌
      org-habit-graph-column 70
      org-habit-following-days 1
      org-habit-show-habits t
      org-habit-completed-glyph ?●
      org-habit-preceding-days 10
      org-habit-show-habits-only-for-today t

      org-habit-missed-glyph ?○)

(with-eval-after-load 'org-habit
  (set-face-attribute 'org-habit-alert-face 'nil :background 'nil)
  (set-face-attribute 'org-habit-alert-future-face 'nil :background 'nil)
  (set-face-attribute 'org-habit-clear-face 'nil :background 'nil)
  (set-face-attribute 'org-habit-clear-future-face 'nil :background 'nil)
  (set-face-attribute 'org-habit-ready-face 'nil :background 'nil)
  (set-face-attribute 'org-habit-ready-future-face 'nil :background 'nil)
  (set-face-attribute 'org-habit-overdue-face 'nil :background 'nil))

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
      '((sequence "TODO(t)" "NEXT(n)" "PROJECT(p)" "|" "DONE(d)" "COMPLETED(c)" "CANCELLED(l)")))
;;'((sequence "TODO(t)" "NEXT(n)" "PROJECT(p)" "WAITING(w)" "MAYBE(m)" "|" "DONE(d)" "COMPLETED(c)" "CANCELLED(l)")))
(setq org-tag-alist '(
                      ("Learn". ?l) ("ACTIVE" . ?a) ("Project".?p) ("Other") ("Daily") ("Book". ?b) ("Class" . ?c)))
(add-hook 'org-agenda-mode-hook #'evil-mode)
;(add-hook 'org-agenda-mode-hook 'org-agenda-log-mode)

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
        ("" "tabularx" t nil)
        ("" "capt-of" nil nil)
        ("colorlinks=true,linkcolor=black,citecolor=blue" "hyperref" t nil)))

(setq org-latex-listings 'minted
      org-latex-prefer-user-labels 't)
(setq org-latex-pdf-process '(
                              ;; "xelatex -interaction nonstopmode %f"
                              ;; "xelatex -interaction nonstopmode %f"
			                  "latexmk -shell-escape -interaction=nonstopmode -bibtex  -pdflatex=%latex -pdf -f %f"
                              "bibtex %b"
			                  "latexmk -shell-escape -interaction=nonstopmode -bibtex  -pdflatex=%latex -pdf -g -f %f"
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

(use-package org-roam
  :straight t
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

;; (use-package org-roam-bibtex
;;   :straight t
;;   :after (org-roam helm-bibtex)
;;   :bind (:map org-mode-map ("C-c n b" . orb-note-actions))
;;   :config
;;   (require 'org-ref)
;;   (org-roam-bibtex-mode))

(setq org-capture-templates '(
                              ("i" "Inbox" entry
                               (file "~/agenda/inbox.org")
                               "* TODO %?\n(Entered on %U)\n"
                               :empty-lines 1)
                              ("m" "Manga" entry
                               (file+headline "~/agenda/manga.org" "Inbox")
                               "* READ %?")))

(defun org-capture-inbox ()
  "Get current file link and create inbox capture"
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))

(use-package org-download
  :ensure t
  :straight t
  :after org)

(use-package org-ref
  :straight t
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

;; (use-package helm-bibtex
;;   :straight t
;;   :after org-roam
;;   :config
;;   (setq bibtex-completion-bibliography bib-files-directory
;;         bibtex-completion-library-path pdf-files-directory
;;         bibtex-completion-pdf-field "File"
;;         bibtex-completion-notes-path org-directory
;;         bibtex-completion-additional-search-fields '(keywords))
;;   :bind
;;   (("C-c n B" . helm-bibtex)))

(use-package org-noter
  :straight t
  :after org-roam
  :config
  (setq org-noter-notes-search-path "~/Uni/bib/pdf"))

;; (use-package pdf-continuous-scroll-mode
;;   :load-path "~/.emacs.d/plugins/pdf-continuous-scroll-mode.el"
;;   :after pdf-tools)

(use-package org-pdftools
  :straight t
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :straight t
  :after org-noter
  :config
  (setq  org-noter-pdftools-use-org-id nil ; Remove ID on PROPERTIES drawer 
         org-noter-pdftools-use-unique-org-id nil) 
  ;; Add a function to straight precise note is inserted
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
  :straight t
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page))
  :config
  (pdf-tools-install))

;; (use-package image-roll
;;   :straight (image-roll :type git :host github :repo "dalanicolai/image-roll.el"))

;; (use-package writeroom-mode
;;   :straight t
;;   :after org
;;   :config
;;   (setq writeroom-fullscreen-effect 'maximized)
;;   (setq writeroom-width 100)
;;   (setq writeroom-mode-line 't))

(use-package olivetti
  :straight t
  :after org
  :config
  (olivetti-set-width 90))

(use-package focus
  :straight t
  :after org)

