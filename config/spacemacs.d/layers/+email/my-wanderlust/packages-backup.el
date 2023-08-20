;;; packages.el --- Wanderlust Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst my-wanderlust-packages
  '(
    (nsm :location built-in)
    org-mime
    persp-mode
    wanderlust
    (elmo-vars :requires wanderlust :location built-in)
    (elmo :requires wanderlust :location built-in)
    (elmo-archive :requires wanderlust :location built-in)
    (elmo-search :requires wanderlust :location built-in)
    (elmo-localdir :requires wanderlust :location built-in)
    (elmo-localnews :requires wanderlust :location built-in)
    (elmo-maildir :requires wanderlust :location built-in)
    (elmo-imap4 :requires wanderlust :location built-in)
    (elmo-passwd :requires wanderlust :location built-in)
    (elmo-split :requires wanderlust :location built-in)
    (wl-vars :requires wanderlust :location built-in)
    (wl-summary :requires wanderlust :location built-in)
    (mime-conf :requires flim :location built-in)
    (mime-view :requires semi :location built-in)
    (semi-setup :requires semi :location built-in)
    (mime-w3m :requires w3m :location built-in)
    ;; w3m
    (shimbun :requires w3m)
    (wl-gravatar :requires wanderlust :location (recipe :fetcher github :repo "dabrahams/wl-gravatar"))
    (x-face-e21 :location local)
    )
  )

(defun my-wanderlust/init-nsm ()
  (use-package nsm
    :if (featurep 'nsm)
    :init
    (setq nsm-settings-file (concat
                             spacemacs-cache-directory
                             "network-security.data")
          nsm-save-host-names t)
    )
  )

(defun my-wanderlust/pre-init-org-mime ()
  (spacemacs|use-package-add-hook org-mime
    :post-config
    (use-package org-mimie
      :init
      (setq org-mime-library 'semi)
      )
    )
  )

(defun my-wanderlust/pre-init-persp-mode ()
  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (add-to-list 'persp-filter-save-buffers-functions
                 'spacemacs//wanderlust-persp-filter-save-buffers-function)
    (spacemacs|define-custom-layout wanderlust-spacemacs-layout-name
      :binding wanderlust-spacemacs-layout-binding
      :body
      (dolist (mode wanderlust-modes)
        (let ((hook (intern (concat (symbol-name mode) "-hook"))))
          (add-hook hook #'spacemacs//wanderlust-buffer-to-persp)))
      (call-interactively 'wanderlust)
      )
    )
  )

(defun my-wanderlust/init-wanderlust ()
  (use-package wl
    :defer t
    :commands (wl wl-other-frame wl-draft wl-user-agent
                  wl-user-agent-compose wl-draft-send
                  wl-draft-kil compose-mail)
    :init
    (spacemacs/declare-prefix "anw" "wanderlust")
    (spacemacs/set-leader-keys
      "anww" 'wl
      "anwm" 'compose-mail)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook)
    (setq read-mail-command 'wl
          mail-user-agent 'wl-user-agent)
    (cond ((file-exists-p (expand-file-name
                           ".authinfo"
                           my-wanderlust-layer-configuration-dir))
           (add-to-list 'auth-sources (concat my-wanderlust-layer-configuration-dir ".authinfo")))
          ((file-exists-p (expand-file-name
                           ".authinfo.gpg"
                           my-wanderlust-layer-configuration-dir))
           (add-to-list 'auth-sources (concat my-wanderlust-layer-configuration-dir ".authinfo.gpg"))))
    :config
    (evilified-state-evilify-map wl-draft-mode-map
      :mode wl-draft-mode)
    (evilified-state-evilify-map wl-folder-mode-map
      :mode wl-folder-mode)
    (evilified-state-evilify-map wl-plugged-mode-map
      :mode wl-plugged-mode)
    (evilified-state-evilify-map wl-summary-mode-map
      :mode wl-summary-mode)
    (evilified-state-evilify-map wl-template-mode-map
      :mode wl-template-mode)
    )
  )

(defun my-wanderlust/init-mime-conf ()
  (use-package mime-conf
    :config
    (setq mime-mailcap-file my-mime-view-private-mailcap-files)
    )
  )

(defun my-wanderlust/init-mime-view ()
  (use-package mime-view
    :hook (mime-display-text/plain-hook . user--mime-display-text/plain-hook)
    :config
    (setq mime-situation-examples-file nil)
    ;; (setq mime-situation-examples-file (concat
    ;;                                     my-wanderlust-layer-configuration-dir "mime-example"))
    ;; (setq mime-view-mailcap-files (append my-mime-view-private-mailcap-files))
    )
  )

(defun my-wanderlust/init-elmo ()
  (use-package elmo
    :config
    ;; Maximum size of message to fetch without confirmation.
    (setq elmo-message-fetch-threshold (* 512 1024))

    (unless (or (executable-find "namazu")
                (executable-find "mu"))
      (message "Namazu or mu not found, mail will not be indexed."))
    )
  )

(defun my-wanderlust/init-elmo-vars ()
  (use-package elmo-vars
    :config
    (setq elmo-msgdb-directory (expand-file-name "elmo" my-wanderlust-cache-directory)
          elmo-cache-directory (expand-file-name "cache" elmo-msgdb-directory)
          elmo-passwd-storage-type 'auth-source)
    )
  )

(defun my-wanderlust/init-elmo-archive ()
  (use-package elmo-archive
    :config
    (setq elmo-archive-folder-path (expand-file-name "Mail" my-wanderlust-cache-directory))
    )
  )

(defun my-wanderlust/init-elmo-search ()
  (use-package elmo-search
    :config
    (setq elmo-search-namazu-default-index-path (expand-file-name "Mail" my-wanderlust-cache-directory))
    (when (executable-find "mu")
      (setq
       ;; Use mu instead of namazu, when available.
       elmo-search-default-engine 'mu
       ;; Default folder name prefix.
       wl-default-spec "[")

      (elmo-search-register-engine
       'mu 'local-file
       :prog "mu"
       :args '("find" pattern "--format" "plain" "--fields" "location")
       :charset 'utf-8))
    )
  )

(defun my-wanderlust/init-elmo-localdir ()
  (use-package elmo-localdir
    :config
    (setq elmo-localdir-folder-path (expand-file-name "Mail" my-wanderlust-cache-directory))
    )
  )

(defun my-wanderlust/init-elmo-localnews ()
  (use-package elmo-localnews
    :config
    (setq elmo-localnews-folder-path (expand-file-name "News" my-wanderlust-cache-directory))
    )
  )

(defun my-wanderlust/init-elmo-maildir ()
  (use-package elmo-maildir
    :config
    (setq elmo-maildir-folder-path (expand-file-name "Maildir" my-wanderlust-cache-directory))
    )
  )

(defun my-wanderlust/init-elmo-imap4 ()
  (use-package elmo-imap4
    :config
    (setq elmo-imap4-use-modified-utf7 t)
    )
  )

(defun my-wanderlust/init-elmo-passwd ()
  (use-package elmo-passwd
    :config
    (setq elmo-passwd-alist-file-name (concat my-wanderlust-layer-configuration-dir "passwd"))
    )
  )

(defun my-wanderlust/init-elmo-split ()
  (use-package elmo-split
    :config
    (setq elmo-split-log-file (concat (file-name-as-directory elmo-msgdb-directory) "split-log"))
    )
  )

(defun my-wanderlust/init-wl-vars ()
  (use-package wl-vars
    :config
    (setq wl-temporary-file-directory (concat my-wanderlust-cache-directory)
          wl-x-face-file (concat my-wanderlust-layer-configuration-dir ".xface")
          wl-init-file (concat my-wanderlust-layer-configuration-dir ".wl")
          wl-folders-file (concat my-wanderlust-layer-configuration-dir ".folders")
          wl-address-file (concat my-wanderlust-layer-configuration-dir ".addresses")
          wl-alias-file (concat my-wanderlust-layer-configuration-dir ".aliases")
          ;; (Wanderlust)
          ;; Mark sent mails as read.
          wl-fcc-force-as-read t
          ;; Check for mail when idle.
          wl-biff-check-interval 180
          wl-biff-use-idle-timer t
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
          wl-message-ignored-field-list '("^.*")
          wl-message-visible-field-list
          '("^\\(To\\|Cc\\):"
            "^Subject:"
            "^\\(From\\|Reply-To\\):"
            "^Organization:"
            "^X-Attribution:"
            "^\\(Posted\\|Date\\):"
            "^X-\\(Face\\(-[0-9]+\\)?\\|Weather\\|Fortune\\|Now-Playing\\):")
          ;; Allow sort on visible fields.
          wl-message-sort-field-list
          '("^Subject:"
            "^From:"
            "^To:"
            "^Cc:"
            "^Date:"
            "^Message-ID:")
          ;; (Drafts)
          ;; Raise a new frame when creating a draft.
          wl-draft-use-frame t
          ;; Automatically save drafts every two minutes.
          wl-auto-save-drafts-interval 120.0
          ;; Sane forward tag.
          wl-forward-subject-prefix "Fwd: "
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
          ;; Display TO rather than FROM in "Sent" folders.
          wl-summary-showto-folder-regexp ".*Sent.*"
          ;; Divide thread if subject has changed.
          wl-summary-divide-thread-when-subject-changed t
          ;; List of marks to display in summary.
          wl-summary-incorporate-marks '("N" "U" "!" "A" "$")
          ;; (Folders)
          ;; Show folders in a pane to the left.
          wl-stay-folder-window nil
          wl-folder-window-width 45
          ;; Asynchronously update folders.
          wl-folder-check-async t
          )

    (with-eval-after-load 'wl-gravatar
      (setq wl-highlight-x-face-function 'wl-gravatar-insert))

    ;; (Hooks)
    ;; Don't apply email account template when sending draft, otherwise switching
    ;; templates won't work.
    (remove-hook 'wl-draft-send-hook 'wl-draft-config-exec)
    )
  )

(defun my-wanderlust/init-wl-summary ()
  (use-package wl-summary
    :config
    ;; Sort threads based on the date of the latest reply.
    (add-to-list 'wl-summary-sort-specs 'reply-date)
    (setq wl-summary-default-sort-spec 'reply-date)

    ;; Set up guides in summary mode.
    (user/wanderlust-set-summary-guides)
    ))

(defun my-wanderlust/init-semi-setup ()
  (use-package semi-setup
    :config
    (setq
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

     (with-eval-after-load 'mime-view
       (cond ((and wanderlust-enable-w3m-preview
                   (executable-find "w3m")
                   (configuration-layer/layer-used-p 'my-w3m)
                   (locate-library (symbol-name 'mime-w3m)))
              (setq mime-browse-url-function 'w3m-browse-url)
              ;; (user--semi-w3m-config)
              ;; (when (fboundp 'imagemagick-register-types)
              ;;   (imagemagick-register-types)
              ;;   (image-type-available-p 'imagemagick)
              ;;   (setq imagemagick-enabled-types t))
              )
             ((and wanderlust-enable-shr-preview
                   ;; (configuration-layer/layer-used-p 'my-eww)
                   (locate-library (symbol-name 'mime-shr)))
              ;; (user--semi-shr-config)
              (setq mime-view-text/html-previewer 'shr
                    mime-shr-blocked-images nil
                    ;; mime-setup-enable-inline-image t
                    mime-browse-url-function 'eww-browse-url)
              ;; (when (fboundp 'imagemagick-register-types)
              ;;   (imagemagick-register-types)
              ;;   (image-type-available-p 'imagemagick)
              ;;   (setq imagemagick-enabled-types t))
              ))

       (set-alist 'mime-view-type-subtype-score-alist '(text . html) 3)
       )
     )
    )
  )

(defun my-wanderlust/init-mime-w3m ()
  (use-package mime-w3m
    :defer t
    :if my-wanderlust-enable-w3m-preview
    :init
    (setq mime-w3m-safe-url-regexp nil)
    ;;       mime-setup-enable-inline-html t)
    ;; (add-hook 'wl-init-hook 'mime-w3m-insinuate)
    )
  )

;; (defun my-wanderlust/post-init-w3m ()
;;   (use-package w3m
;;     :if my-wanderlust-enable-w3m-preview
;;     :config
;;     (setq w3m-mailto-url-function 'wl-draft)
;;     )
;;   )

(defun my-wanderlust/init-shimbun ()
  (use-package shimbun
    :defer t
    )
  )

(defun my-wanderlust/init-wl-gravatar ()
  (use-package wl-gravatar
    :init
    (autoload 'wl-gravatar-insert "wl-gravatar")
    )
  )

(defun my-wanderlust/init-x-face-e21 ()
  (use-package x-face-e21
    :defer t
    :if my-wanderlust-enable-x-face
    :init
    (autoload 'x-face-decode-message-header "x-face-e21")
    (setq wl-highlight-x-face-function 'x-face-decode-message-header)
    )
  )


;;; packages.el ends here
