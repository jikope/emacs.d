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
  :straight t
  :hook (mu4e-mode . mu4e-alert-enable-mode-line-display)
  :config
  ;;(mu4e-alert-enable-mode-line-display)
  (mu4e-alert-set-default-style 'libnotify)
  (setq mu4e-alert-notify-repeated-mails t)
  (setq mu4e-alert-enable-notifications t))
