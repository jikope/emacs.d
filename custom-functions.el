;;; custom-functions.el --- Custom Functions Library -*- lexical-binding: t; -*-
;;; Code:

(defvar jikope/music-genres '("Jejepangan" "Gamelan" "SERA" "Jazz" "Keroncong" "Gachi" "Reggae")
  "List of music genres that i listen to")

(defun jikope/youtube-to-mp3 (link)
  "Download a youtube video to mp3 format given LINK."
  (interactive "sEnter youtube link: ")
  (let (
        (download_path "/media/data/bima/lagu/")
        (download_options "-x -f \'ba\' --audio-format mp3")
        (current_date (format-time-string "%Y-%m-%d")))
    (async-shell-command (concat "yt-dlp \"" link "\" -o \"" download_path current_date " %(title)s.%(ext)s\" " download_options))))

(defun copy-current-path ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun jikope/kill-and-close-window ()
  "Kill current buffer and close window."
  (interactive)
  (kill-this-buffer)
  (delete-window))

(defun org-retrieve-url-from-point ()
  "Copies the URL from an org link at the point"
  (interactive)
  (let ((plain-url (url-get-url-at-point)))
    (if plain-url
        (progn
          (kill-new plain-url)
          (message (concat "Copied: " plain-url)))
      (let* ((link-info (assoc :link (org-context)))
             (text (when link-info
                     (buffer-substring-no-properties
                      (or (cadr link-info) (point-min))
                      (or (caddr link-info) (point-max))))))
        (if (not text)
            (error "Oops! Point isn't in an org link")
          (string-match org-link-bracket-re text)
          (let ((url (substring text (match-beginning 1) (match-end 1))))
            (kill-new url)
            (message (concat "Copied: " url))))))))

;; https://emacs.stackexchange.com/a/60555
(defun farynaio/org-link-copy (&optional arg)
  "Extract URL from org-mode link and add it to kill ring."
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
          (type (org-element-property :type link))
          (url (org-element-property :path link))
          (url (concat type ":" url)))
    (kill-new url)
    (message (concat "Copied URL: " url))))

;; https://blog.lambda.cx/posts/emacs-align-columns/
(defun align-non-space (BEG END)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t))



(provide 'custom-function)
;;; custom-functions.el ends here
