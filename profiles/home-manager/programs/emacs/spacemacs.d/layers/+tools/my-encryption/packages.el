;;; packages.el --- Encryption Layer packages File for Spacemacs
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

(defconst my-encryption-packages
  '(
    (epa-file :location built-in
              :toggle file-enable-epa-support)
    (epa :location built-in
         :toggle file-enable-epa-support)
    (pinentry :toggle org-enable-pinentry-support)
    )
  )

(defun my-encryption/init-epa-file ()
  (use-package epa-file
    :if file-enable-epa-support
    :defer t
    :config
    (progn
      ;; Enable epa, so I can use gnupg in emacs to en/decrypt file
      (epa-file-enable)
      ;; 总是使用对称加密
      (setq epa-file-encrypt-to nil)
      ;; Control whether or not to pop up the key selection dialog.
      ;; (setq epa-file-inhibit-auto-save t)
      (setq epa-file-select-keys 0)
      ;; Cache passphrase for symmetric encryption.
      (setq epa-file-cache-passphrase-for-symmetric-encryption t)
      ;; non-GUI password dialog. Test: (getenv "GPG_AGENT_INFO")
      (setenv "GPG_AGENT_INFO" nil)
      ;; Stop EasyPG from asking for the recipient used for encrypting files.
      ;; (setq epa-file-encrypt-to "xxx@xxx.com")
      )
    )
  )

(defun my-encryption/init-epa ()
  (use-package epa
    :if file-enable-epa-support
    :init
    (progn
      (setq epg-gpg-home-directory
            (expand-file-name
             (concat user-home-directory (file-name-as-directory ".config") "gnupg")))
      )
    :config
    (progn
      (setq epa-pinentry-mode 'loopback)
      )
    )
  )

(defun my-encryption/init-pinentry ()
  (use-package pinentry
    :if org-enable-pinentry-support
    :config
    (progn
      ;; Start the Pinentry service
      (pinentry-start)
      )
    )
  )


;;; packages.el ends here
