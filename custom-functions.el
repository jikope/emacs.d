;;; custom-functions.el --- Custom Functions Library -*- lexical-binding: t; -*-
;;; Code:

(defun jikope/youtube-to-mp3 (link)
  "Download a youtube video to mp3 format given LINK."
  (interactive "sEnter youtube link: ")
  (let (
        (download_path "/media/data/bima/lagu/")
        (download_options "-x -f \'ba\' --audio-format mp3")
        (current_date (format-time-string "%Y-%m-%d")))
    (async-shell-command (concat "yt-dlp \"" link "\" -o \"" download_path current_date " %(title)s.%(ext)s\" " download_options))))

(defun prelude-copy-file-name-to-clipboard ()
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

(provide 'custom-function)
;;; custom-functions.el ends here
