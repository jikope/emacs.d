;; Use dirvish

(use-package dirvish
  :straight t
  :hook (after-init . dirvish-override-dired-mode)
  :config
(setq dired-listing-switches
      "-l --almost-all --human-readable --group-directories-first --no-group")
(setq dirvish-attributes
      '(;; vc-state git-msg
        subtree-state collapse all-the-icons file-time file-size))
(setq dirvish-default-layout '(0 0.4 0.6)
      dirvish-mode-line-height 20
      dirvish-header-line-height 20)
(set-face-attribute 'header-line nil :font jikope/variable-font :height 120 :weight 'bold)

;; (dirvish-override-dired-mode)

(require 'evil-collection)
(evil-collection-define-key 'normal 'dired-mode-map "q" 'jikope/close-peep-dired-when-up-dir)
(evil-collection-define-key 'normal 'dired-mode-map (kbd "<tab>") 'dirvish-subtree-toggle)
;; (bind-key "q" #'jikope/close-peep-dired-when-up-dir dired-mode-map)
;; (bind-key "q" #'jikope/close-peep-dired-when-up-dir dirvish-mode-map)
(bind-key "S-q" #'dirvish-quit dired-mode-map)
(bind-key "TAB" #'dirvish-subtree-toggle dirvish-mode-map)
(add-hook 'dirvish-find-entry-hook
          (lambda (&rest _) (setq-local truncate-lines t))))

;; https://emacs.stackexchange.com/a/64591
(defun custom/get-dired-marked-files()
  "Return a list of marked files from all Dired buffers."
  (let ((files  ())
        (here   ()))
    (dolist (buf  (mapcar #'cdr dired-buffers))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq here  (dired-get-marked-files nil nil nil t)))
        (when (or (null (cdr here))  (eq t (car here)))
          (setq here  (cdr here)))
        (setq files  (nconc here files))))
    (setq files  (delete-dups files))))

(defun jikope/copy-current-path ()
  "Returns the current dired buffer path."
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      filename)
    ))

(defun jikope/copy-marked-files()
  "Copy files marked by dired into another dired buffer."
  (interactive)
  (when (eq major-mode #'dired-mode)
    (let ((marked-files (mapconcat 'identity (custom/get-dired-marked-files) " "))
          (target-path (jikope/copy-current-path)))
      (if (and (eq marked-files nil) (eq target-path nil))
          (message "No files marked")
        (save-window-excursion
          (shell-command (format "cp -r %s %s" marked-files target-path)))
        (revert-buffer)))))

(defun jikope/move-marked-files()
  "Move files marked by dired into another dired buffer."
  (interactive)
  (when (eq major-mode #'dired-mode)
    (let ((marked-files (mapconcat 'identity (custom/get-dired-marked-files) " "))
          (target-path (jikope/copy-current-path)))
      (if (eq marked-files nil)
          (message "No files marked")
        (save-window-excursion
          (shell-command (format "mv %s %s" marked-files target-path)))
        (revert-buffer)))))

(use-package dired-single
  :straight t)

(defun jikope/close-peep-dired-when-up-dir ()
  "Close peep dired when q pressed"
  (interactive)
  ;; (peep-dired-kill-buffers-without-window)
  (dired-single-buffer ".."))
  ;; (dired-find-file ".."))
;; (when (bound-and-true-p peep-dired)
;;   (peep-dired)))

;; (use-package dired
;;   :straight nil
;;   :after evil
;;                                         ;:hook (dired-mode . dired-hide-details-mode)
;;   :custom
;;   (dired-listing-switches "-alh --group-directories-first")
;;   (dired-dwim-target 't)
;;   :config
;;   (require 'evil-collection)
;;   (evil-collection-define-key 'normal 'dired-mode-map
;;     (kbd "RET") 'dired-single-buffer
;;                                         ;"q" 'jikope/close-peep-dired-when-up-dir))
;;     "q" 'jikope/close-peep-dired-when-up-dir))

;; ;; (use-package all-the-icons-dired
;; ;;   :straight t
;; ;;   :after dired

;; ;; FIX: Error cause by SVG tag mode
;; ;; (add-to-list 'font-lock-extra-managed-props 'display) ; DISABLED BECAUSE DIRED ICON
;; (use-package all-the-icons-dired
;;   :straight (all-the-icons-dired :type git :host github :repo "jtbm37/all-the-icons-dired"
;;                                  :fork (:host github
;;                                               :repo "wyuenho/all-the-icons-dired"))
;;   :after dired
;;   :hook (dired-mode . all-the-icons-dired-mode))

;; ;; (use-package dired-details-r
;; ;;   :straight (dired-details-r :type git :host github :repo "misohena/dired-details-r")
;; ;;   :after dired
;; ;;   :init
;; ;;   (dired-details-r-setup))
;; ;;   (setq dired-details-r-combinations
;; ;;         '((size-time  . (size))
;; ;;           (no-details . ())
;; ;;           (disabled   . disabled)
;; ;;           (all        . (time links user group)))))

;; (use-package dired-sidebar
;;   :straight t
;;   :after dired
;;   :bind (("<f3>" . dired-sidebar-toggle-sidebar))
;;   :ensure t
;;   :commands (dired-sidebar-toggle-sidebar)
;;   :init
;;   (add-hook 'dired-sidebar-mode-hook
;;             (lambda ()
;;               (visual-line-mode -1)
;;               (unless (file-remote-p default-directory)
;;                 (auto-revert-mode))))
;;   :config
;;   (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
;;   (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

;;   (setq dired-sidebar-subtree-line-prefix "  "
;;         dired-sidebar-theme 'icons
;;         dired-sidebar-width 28
;;         dired-sidebar-use-term-integration t
;;         dired-sidebar-use-custom-font t))

;; (use-package diredfl
;;   :straight t
;;   :hook (dired-mode . diredfl-mode)
;;   :config
;;   (set-face-attribute 'diredfl-dir-name 'nil :background nil))

;; (use-package peep-dired
;;   :straight t
;;   :after evil
;;   :config
;;   (setq peep-dired-cleanup-on-disable t)
;;   ;;(setq peep-dired-enable-on-directories t)
;;   (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4" "mp3" "webm" "zip"))
;;   (evil-define-key 'normal
;;     peep-dired-mode-map 
;;     (kbd "j") 'peep-dired-next-file
;;     (kbd "k") 'peep-dired-prev-file)
;;   (add-hook 'peep-dired-hook 'evil-normalize-keymaps))

;; ;; (use-package dired-posframe
;; ;;   :straight t
;; ;;   :after dired
;; ;;   :hook (dired-mode . dired-posframe-mode)
;; ;;   :custom
;; ;;   (dired-posframe-min-height 20)
;; ;;   (dired-posframe-min-width 40)
;; ;;   (dired-posframe-width 80)
;; ;;   (dired-posframe-height 20)
;; ;;   (dired-posframe-file-size-limit (* 5 1024 1024)))

;; (use-package dired-rainbow
;;   :straight t
;;   :after dired
;;   :config
;;   (progn
;;     (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
;;     (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
;;     (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
;;     (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
;;     (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
;;     (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
;;     (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
;;     (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
;;     (dired-rainbow-define log "#c17d11" ("log"))
;;     (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
;;     (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
;;     (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
;;     (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
;;     (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
;;     (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
;;     (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
;;     (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
;;     (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
;;     (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
;;     (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
;;     ))

;; ;; (use-package dired-sidebar
;; ;;   :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
;; ;;   :straight t
;; ;;   :commands (dired-sidebar-toggle-sidebar)
;; ;;   :init
;; ;;   (add-hook 'dired-sidebar-mode-hook
;; ;;             (lambda ()
;; ;;               (unless (file-remote-p default-directory)
;; ;;                 (auto-revert-mode))))
;; ;;   :config
;; ;;   (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
;; ;;   (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

;;   ;; (setq dired-sidebar-subtree-line-prefix "__")
;;   ;; (setq dired-sidebar-theme 'vscode)
;;   ;; (setq dired-sidebar-use-term-integration t)
;;   ;; (setq dired-sidebar-use-custom-font t))
