;;; packages.el --- Mu4e Layer packages File for Spacemacs
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

(defconst my-mu4e-packages
  '(
    (mu4e :location site)
    org-mime
    )
  )

(defun my-mu4e/post-init-mu4e ()
  (use-package mu4e
    :config
    (progn
      ;; Set up some common mu4e variables
      (setq mu4e-maildir "~/.cache/Maildir/Tencent"
            mu4e-drafts-folder "/Drafts"
            mu4e-sent-folder   "/Sent Messages"
            mu4e-refile-folder "/Archive"
            mu4e-trash-folder "/Deleted Messages"
            mu4e-get-mail-command "offlineimap"
            mu4e-update-interval nil
            mu4e-compose-signature-auto-include nil
            ;; mu4e-view-prefer-html t
            mu4e-view-show-addresses t)

      (setq mu4e-maildir-shortcuts
            '(("/INBOX" . ?i)
              ("/Sent Messages" . ?s)
              ("/Junk" . ?j)
              ("/Deleted Messages" . ?d)
              ))

      (cond ((and mu4e-enable-html2text-preview
                  (executable-find "html2text"))
             (setq mu4e-html2text-command "html2text -utf8 -nobs -width 72"))
            ((and mu4e-enable-w3m-preview
                  (executable-find "w3m"))
             (setq mu4e-html2text-command "w3m -dump -cols 80 -T text/html"))
            ((and mu4e-enable-textutil-preview
                  (executable-find "textutil"))
             (setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout"))
            ;; This one gives live links, but seems to be slow.
            ((and mu4e-enable-shr-preview
                  (configuration-layer/layer-used-p 'my-eww)
                  (featurep 'eww))
             (setq mu4e-html2text-command 'mu4e-shr2text))
            )

      ;; (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
      ;; (setq mu4e-user-mail-address-list
      ;;       (mapcar (lambda (account) (cadr (assq 'user-mail-address account)))
      ;;               my-mu4e-account-alist))

      ;; Bookmarks
      (setq mu4e-bookmarks
            `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
              ("date:today..now" "Today's messages" ?t)
              ("date:7d..now" "Last 7 days" ?w)
              ("mime:image/*" "Messages with images" ?p)
              (,(mapconcat 'identity
                           (mapcar
                            (lambda (maildir)
                              (concat "maildir:" (car maildir)))
                            mu4e-maildir-shortcuts) " OR ")
               "All inboxes" ?i)))
      )
    )
  )

(defun my-mu4e/pre-init-org-mime ()
  (spacemacs|use-package-add-hook org-mime
    :post-config
    (use-package org-mimie
      :init
      (setq org-mime-library 'mml))))


;;; packages.el ends here
