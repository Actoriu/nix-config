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
    (helm-wl-address :requires helm
                     :location (recipe
                                :fetcher github
                                :repo "kenbeese/helm-wl-address"))
    (nsm :location built-in)
    org
    org-mime
    persp-mode
    (shr :location built-in)
    wanderlust
    w3m
    window-purpose
    (shimbun :toggle my-wanderlust-enable-w3m-preview)
    (supercite :location built-in)
    (x-face-e21 :location local)
    )
  )

(defun my-wanderlust/init-helm-wl-address ()
  (use-package helm-wl-address
    :defer t
    ;; :init
    ;; (add-hook 'wl-draft-mode-hook 'helm-wl-address-activate-tab)
    ))

(defun my-wanderlust/init-nsm ()
  (use-package nsm
    :if (featurep 'nsm)
    :init
    (setq nsm-settings-file (concat
                             spacemacs-cache-directory
                             "network-security.data")
          nsm-save-host-names t)))

(defun my-wanderlust/pre-init-org ()
  (if my-wanderlust-enable-org
      (spacemacs|use-package-add-hook org
        :post-config (require 'org-wl))))

(defun my-wanderlust/pre-init-org-mime ()
  (if my-wanderlust-enable-org
      (spacemacs|use-package-add-hook org-mime
        :post-config
        (use-package org-mimie
          :init
          (setq org-mime-library 'semi)))))

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
      (call-interactively 'wanderlust))))

(defun my-wanderlust/init-shr ()
  (use-package shr
    :if my-wanderlust-enable-shr-preview
    :defer t
    :init
    (setq shr-use-colors nil
          shr-use-fonts nil)))

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

    (if (fboundp 'define-mail-user-agent)
        (define-mail-user-agent
          'wl-user-agent
          'wl-user-agent-compose
          'wl-draft-send
          'wl-draft-kill
          'mail-send-hook))

    (let* ((my-wanderlust-dir
            (configuration-layer/get-layer-local-dir 'my-wanderlust))
           (my-wanderlust-layer-configuration-dir (file-name-as-directory
                                                   (expand-file-name
                                                    "wanderlust"
                                                    my-wanderlust-dir)))
           (my-mime-view-private-mailcap-files (concat
                                                my-wanderlust-layer-configuration-dir
                                                ".mailcap"))
           (my-wanderlust-cache-directory (file-name-as-directory
                                           (expand-file-name
                                            "wanderlust"
                                            spacemacs-cache-directory))))


      ;; elmo-archive (elmo)
      (setq elmo-archive-folder-path (expand-file-name
                                      "Mail"
                                      my-wanderlust-cache-directory))
      ;; elmo-maildir (elmo)
      (setq elmo-maildir-folder-path (expand-file-name
                                      "Maildir"
                                      my-wanderlust-cache-directory))
      ;; elmo-vars (elmo)
      (setq elmo-msgdb-directory (expand-file-name
                                  "elmo"
                                  my-wanderlust-cache-directory)
            elmo-cache-directory (expand-file-name
                                  "cache"
                                  elmo-msgdb-directory))
      (when my-wanderlust-passwd-enable-auth-source
        (setq elmo-passwd-storage-type 'auth-source)
        (cond ((file-exists-p (expand-file-name
                               ".authinfo"
                               my-wanderlust-layer-configuration-dir))
               (add-to-list 'auth-sources (concat my-wanderlust-layer-configuration-dir ".authinfo")))
              ((file-exists-p (expand-file-name
                               ".authinfo.gpg"
                               my-wanderlust-layer-configuration-dir))
               (add-to-list 'auth-sources (concat my-wanderlust-layer-configuration-dir ".authinfo.gpg")))))
      ;; elmo-localdir (elmo)
      (setq elmo-localdir-folder-path (expand-file-name
                                       "Mail"
                                       my-wanderlust-cache-directory))
      ;; elmo-localnews (elmo)
      (setq elmo-localnews-folder-path (expand-file-name
                                        "News"
                                        my-wanderlust-cache-directory))
      ;; elmo-search (elmo)
      (setq elmo-search-namazu-default-index-path (expand-file-name
                                                   "Mail"
                                                   my-wanderlust-cache-directory))
      ;; elmo-split (elmo)
      (setq elmo-split-log-file (concat (file-name-as-directory
                                         elmo-msgdb-directory) "split-log"))
      ;; elmo-passwd (elmo)
      (setq elmo-passwd-alist-file-name (concat my-wanderlust-layer-configuration-dir "passwd"))
      ;; wl-vars (wl)
      (setq wl-address-file (concat my-wanderlust-layer-configuration-dir ".addresses")
            wl-alias-file (concat my-wanderlust-layer-configuration-dir ".aliases")
            wl-init-file (concat my-wanderlust-layer-configuration-dir "wl.el")
            wl-folders-file (concat my-wanderlust-layer-configuration-dir "folders.wl")
            wl-temporary-file-directory (concat my-wanderlust-cache-directory)
            wl-x-face-file (concat my-wanderlust-layer-configuration-dir ".xface"))
      ;; mime-conf (flim)
      (setq mime-mailcap-file my-mime-view-private-mailcap-files)
      ;; smtpmail (flim)
      (setq smtpmail-queue-dir (file-name-as-directory
                                (expand-file-name
                                 "queued-mail"
                                 elmo-archive-folder-path)))
      ;; mime-view (semi)
      (setq mime-situation-examples-file nil)
      )

    (if (boundp 'mail-user-agent)
        (setq mail-user-agent 'wl-user-agent))
    (if (boundp 'read-mail-command)
        (setq read-mail-command 'wl))

    (cond ((and my-wanderlust-enable-w3m-preview
                (executable-find "w3m")
                (configuration-layer/layer-used-p 'my-w3m)
                (featurep 'w3m)
                (locate-library (symbol-name 'mime-w3m)))
           (setq mime-browse-url-function 'w3m-browse-url)
           )
          ((and my-wanderlust-enable-shr-preview
                (configuration-layer/layer-used-p 'my-eww)
                (featurep 'eww)
                (locate-library (symbol-name 'mime-shr)))
           (setq mime-view-text/html-previewer 'shr
                 mime-shr-blocked-images nil
                 mime-browse-url-function 'eww-browse-url)
           ))
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

(defun my-wanderlust/post-init-w3m ()
  (use-package w3m
    :if my-wanderlust-enable-w3m-preview
    :init
    (use-package mime-w3m
      :defer t
      :init
      (progn
        (setq mime-w3m-safe-url-regexp nil)))))

(defun my-wanderlust/pre-init-window-purpose ()
  (spacemacs|use-package-add-hook window-purpose
    :pre-config
    (dolist (mode wanderlust-modes)
      (add-to-list 'purpose-user-mode-purposes (cons mode 'mail)))))

(defun my-wanderlust/init-shimbun ()
  (use-package shimbun
    :defer t))

(defun my-wanderlust/init-supercite()
  (use-package supercite
    :defer t
    :init
    (setq sc-attrib-selection-list nil
          sc-auto-fill-region-p nil
          sc-blank-lines-after-headers 1
          sc-citation-delimiter-regexp "[>]+\\|\\(: \\)+"
          ;; sc-citation-leader ""
          sc-cite-blank-lines-p nil
          sc-confirm-always-p nil
          sc-electric-references-p nil
          sc-fixup-whitespace-p t
          sc-nested-citation-p nil
          sc-preferred-header-style 4
          sc-use-only-preference-p nil)
    :config
    (defun my-sc-header ()
      "Insert `Dear <sc-select-attribution>,' at the beginning of replies."
      (let ((sc-mumble "")
            (whofrom (sc-whofrom)))
        (if whofrom
            (insert "Dear " (sc-select-attribution) ","))))

    (add-to-list 'sc-rewrite-header-list '(my-sc-header) t)

    ;; Add supercite support
    (add-hook 'mail-citation-hook 'sc-cite-original)))

(defun my-wanderlust/init-x-face-e21 ()
  (use-package x-face-e21
    :if my-wanderlust-enable-x-face
    :defer t))


;;; packages.el ends here
