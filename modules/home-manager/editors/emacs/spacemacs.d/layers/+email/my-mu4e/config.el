;;; config.el --- Mu4e configuration File for Spacemacs
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

(defvar mu4e-enable-w3m-preview nil
  "If non nil mime preview setup w3m are enabled.")

(defvar mu4e-enable-shr-preview nil
  "If non nil mime preview setup eww are enabled.")

(defvar mu4e-enable-html2text-preview nil
  "If non nil mime preview setup html2text are enabled.")

(defvar mu4e-enable-textutil-preview nil
  "If non nil mime preview setup textutil are enabled.")

;; Multiple Accounts
(defvar my-mu4e-account-alist
  '(("Tencent"
     (user-mail-address "864149939@qq.com")
     (smtpmail-default-smtp-server "smtp.qq.com")
     (smtpmail-local-domain "qq.com")
     (smtpmail-smtp-user "864149939")
     (smtpmail-smtp-server "smtp.qq.com")
     (smtpmail-stream-type 'starttls)
     (smtpmail-smtp-service 587))
    ;; ("Sina"
    ;;  (mu4e-maildir "~/.cache/Maildir/Sina")
    ;;  (user-mail-address "gclandsoft@sina.com")
    ;;  (smtpmail-default-smtp-server "smtp.sina.com")
    ;;  (smtpmail-local-domain "sina.com")
    ;;  (smtpmail-smtp-user "gclandsoft@sina.com")
    ;;  (smtpmail-smtp-server "smtp.sina.com")
    ;;  (smtpmail-stream-type 'starttls)
    ;;  (smtpmail-smtp-service 587))
    ))


;;; config.el ends here
