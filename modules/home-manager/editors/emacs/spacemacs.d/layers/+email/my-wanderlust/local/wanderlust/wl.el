;;; Demo(启动画面)
(cond ((unless (display-graphic-p))
       (setq wl-demo-display-logo nil
             wl-highlight-folder-with-icon nil)))

;;; Coding
(setq charsets-mime-charset-alist
      (cons
       (cons (list 'unicode) 'utf-8)
       charsets-mime-charset-alist))

(add-hook 'wl-draft-mode-hook
          (lambda ()
            (add-to-list 'mime-charset-type-list '(utf-8 8 nil))))

(setq-default mime-transfer-level 8)
(setq default-mime-charset-for-write 'utf-8
      mime-header-accept-quoted-encoded-words t)

;;; SMTP(邮件发送)
;; 单独设置wl-from，取消wanderlust启动时烦人的错误提示。
;; (setq wl-from (concat user-full-name " <" user-mail-address ">"))
(setq wl-from "10000 <864149939@qq.com>")

(setq wl-default-folder "%INBOX"
      wl-draft-folder "+Drafts"
      wl-trash-folder "+Trash"
      wl-spam-folder "+Spam"
      wl-queue-folder "+Queue"
      elmo-lost+found-folder "+Lost+Found")

;; (setq wl-draft-send-mail-function 'wl-draft-send-mail-with-sendmail
;;       sendmail-program "/usr/bin/msmtp")

;; User-Agent
(setq wl-generate-mailer-string-function
      'wl-generate-user-agent-string-1)

;; Multiple Accounts sendmail
(setq wl-user-mail-address-list
      ;; (list (wl-address-header-extract-address wl-from)
      '("864149939@qq.com"
        "gclandsoft@sina.com"))

(setq wl-template-default-name "Tencent")

(setq wl-template-alist
      '(("Tencent"
         (wl-from . "10000 <864149939@qq.com>")
         ("From" . wl-from)
         (wl-smtp-posting-user . "864149939")
         (wl-smtp-posting-server . "smtp.qq.com")
         (wl-smtp-authenticate-type ."login")
         (wl-smtp-connection-type . 'ssl)
         (wl-smtp-posting-port . 465)
         (wl-local-domain . "qq.com")
         (wl-message-id-domain . "qq.com")
         )
        ("Sina"
         (wl-from . "10000 <gclandsoft@sina.com>")
         ("From" . wl-from)
         (wl-smtp-posting-user . "gclandsoft@sina.com")
         (wl-smtp-posting-server . "smtp.sina.com")
         (wl-smtp-authenticate-type ."login")
         (wl-smtp-connection-type . 'ssl)
         (wl-smtp-posting-port . 465)
         (wl-local-domain . "sina.com")
         (wl-message-id-domain . "sina.com")
         )
        )
      )

(setq wl-draft-config-alist
      '(
        ((string-match "sina.com" wl-draft-parent-folder)
         (template . "Sina"))
        ((string-match "qq.com" wl-draft-parent-folder)
         (template . "Tencent"))
        ;; automatic for replies
        (reply "\\(To\\|Cc\\|Delivered-To\\): .*sina.com.*"
               (template . "Sina"))
        (reply "\\(To\\|Cc\\|Delivered-To\\): .*qq.com.*"
               (template . "Tencent"))
        ))

(remove-hook 'wl-draft-send-hook 'wl-draft-config-exec)
(add-hook 'wl-draft-send-hook
          (lambda ()
            (set (make-local-variable 'wl-from)
                 (std11-fetch-field "From"))))

(add-hook 'wl-mail-setup-hook
          '(lambda ()
             (unless wl-draft-reedit ;; don't apply when reedit.
               (wl-draft-config-exec wl-draft-config-alist))))

;; Folders
;; (setq wl-folder-desktop-name "Email")
(setq my-wl-default-filing-folder ".***"
      wl-default-spec "%"
      wl-folder-hierarchy-access-folders
      '("^.\\([^/.]+[/.]\\)*[^/.]+\\(:\\|@\\|$\\)"
        "^-[^.]*\\(:\\|@\\|$\\)"
        "^@$"
        "^'$")

      ;; backup folders files
      wl-interactive-save-folders nil
      wl-fldmgr-make-backup nil

      ;; wl-auto-check-folder-name "%INBOX"
      ;; wl-dispose-folder-alist '(("^." . remove))
      ;; wl-auto-check-folder-list '("^\\.")
      wl-auto-uncheck-folder-list '("^@")
      wl-auto-select-next 'unread

      wl-folder-notify-deleted 'sync
      wl-fldmgr-add-complete-with-current-folder-list t

      ;; Don't split large mails.
      mime-edit-split-message nil
      ;; Decrypt encrypted emails automatically.
      mime-pgp-decrypt-when-preview t
      ;; MIME type priorities.
      mime-view-type-subtype-score-alist
      '(((text . plain) . 4)
        ((text . enriched) . 3)
        ((text . html) . 2)
        ((text . richtext) . 1))

      ;; Mark sent mails as read.
      wl-fcc-force-as-read t
      wl-biff-check-folder-list '("%INBOX")
      ;; Check for mail when idle.
      wl-biff-check-interval 180
      ;; Set notification function.
      wl-biff-notify-hook 'user--wanderlust-notify-hook
      ;; Let SMTP server handle Message-ID.
      wl-insert-message-id nil
      ;; Quit without asking.
      wl-interactive-exit nil
      ;; (Modeline)
      ;; Show mail status in mode line.
      global-mode-string (cons '(wl-modeline-biff-status
                                 wl-modeline-biff-state-on
                                 wl-modeline-biff-state-off) global-mode-string)
      ;; (Messages)
      ;; Message window size.
      wl-message-window-size '(1 . 3)
      ;; Field lists.
      wl-message-ignored-field-list '("^.*:")
      mime-view-ignored-field-list wl-message-ignored-field-list
      wl-message-visible-field-list
      '("^\\(To\\|Cc\\):"
        "^Subject:"
        "^\\(From\\|Reply-To\\):"
        "^Organization:"
        "^X-Attribution:"
        "^\\(Posted\\|Date\\):"
        "^\\(User-Agent\\|X-Mailer\\):"
        "^X-\\(Face\\(-[0-9]+\\)?\\|Weather\\|Fortune\\|Now-Playing\\):")
      ;; Allow sort on visible fields.
      wl-message-sort-field-list
      '("^Subject:"
        "^From:"
        "^Organization:"
        "^X-Attribution:"
        "^To:"
        "^Cc:"
        "^Date:"
        "^Message-ID:")
      ;; (Drafts)
      ;; Automatically save drafts every two minutes.
      wl-auto-save-drafts-interval 120.0
      ;; Automatically select the correct template based on folder.
      wl-draft-config-matchone t
      ;; (Summary)
      ;; Set verbose summary.
      wl-summary-width nil
      wl-summary-line-format
      "%T%P │%Y-%M-%D %h:%m│ %17(%f%) │%-4S│%4i│%1@ %t%~%c%~%#%~%s "
      wl-folder-summary-line-format-alist
      '(("^+" . "%n%T%P │%Y-%M-%D %h:%m│ %17(%f%) │%-4S││%4i│%1@ %t%~%c%~%#%~%s ")
        ("^file:" . "%T%P %17f %-5S %Y/%M/%D(%W) %h:%m %s "))
      ;; Format of mode-line entry.
      wl-summary-mode-line-format "WL:%n/%u/%a{%t}%f"
      ;; Display TO rather than FROM in "." folders.
      wl-summary-showto-folder-regexp ".*"
      ;; List of marks to display in summary.
      wl-summary-incorporate-marks '("N" "U" "!" "A" "$")
      )

;; search email
(if (and (executable-find "notmuch")
         (configuration-layer/layer-used-p 'notmuch))
    (elmo-search-register-engine 'notmuch-custom 'local-file
                                 :prog "notmuch-query-custom"
                                 :args '(elmo-search-split-pattern-list)
                                 :charset 'utf-8)
  (setq elmo-search-default-engine 'notmuch-custom
        wl-quicksearch-folder "[]"))

(if (and (executable-find "xapian-config")
         my-wanderlust-enable-search-cjk)
    (setenv "XAPIAN_CJK_NGRAM" "1"))

;; (Folders)
;; Show folders in a pane to the left.
(if my-wanderlust-enable-stay-folder-window
    (setq wl-stay-folder-window t))
(if wl-stay-folder-window
    (setq wl-folder-window-width 26))

;; Sort threads based on the date of the latest reply.
(when (functionp 'wl-summary-overview-entity-compare-by-reply-date)
  (add-to-list 'wl-summary-sort-specs 'reply-date)
  (setq wl-summary-default-sort-spec 'reply-date))

;; Fancy look
(if (eq default-terminal-coding-system 'utf-8)
    (setq wl-thread-indent-level 2
          wl-thread-have-younger-brother-str "├─┬─"
          wl-thread-vertical-str             "│"
          wl-thread-youngest-child-str       "╰───"
          wl-thread-horizontal-str           "─"
          wl-thread-space-str                " ")
  (setq wl-thread-indent-level 2
        wl-thread-have-younger-brother-str "+"
        wl-thread-youngest-child-str       "+"
        wl-thread-vertical-str             "|"
        wl-thread-horizontal-str           "-"
        wl-thread-space-str                " "))

;; x-face
(when window-system
  (cond ((and (featurep 'xemacs)	;; for XEmacs
              (module-installed-p 'x-face)
              my-wanderlust-enable-x-face)
         (autoload 'x-face-xmas-wl-display-x-face "x-face")
         (setq wl-highlight-x-face-function 'x-face-xmas-wl-display-x-face))

        ;; for Emacs21
        ((and (not (featurep 'xemacs))
              (>= emacs-major-version 21)
              (module-installed-p 'x-face-e21)
              my-wanderlust-enable-x-face)
         (autoload 'x-face-decode-message-header "x-face-e21")
         (setq wl-highlight-x-face-function 'x-face-decode-message-header))

        ;; for Emacs 19.34, Emacs 20.x
        ((and (< emacs-major-version 21)
              (module-installed-p 'x-face-mule)
              my-wanderlust-enable-x-face)
         ;; x-face-mule distributed with bitmap-mule 8.0 or later
         (autoload 'x-face-decode-message-header "x-face-mule")
         (setq wl-highlight-x-face-function 'x-face-decode-message-header))
        ))

;;; News
(setq shimbun-rss-hash-group-path-alist
      '(("EC_topic" "https://emacs-china.org/latest.rss")
        ("EC_posts" "https://emacs-china.org/posts.rss")))

(setq shimbun-atom-hash-group-path-alist
      '(("Chris" "https://chriszheng.science/atom.xml")
        ("S_address" "https://dizhi.logdown.com/posts.atom")))


;;; wl-init-file ends here
