;;; packages.el --- Display Layer packages File for Spacemacs
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

(defconst my-display-packages
  '(
    (shrface :toggle my-display-enable-shrface)
    (shr-tag-pre-highlight :toggle my-display-enable-shrface)
    (inherit-org :location (recipe
                            :fetcher github
                            :repo "chenyanming/inherit-org")
                 :toggle my-display-enable-inherit-org)
    )
  )

(defun my-display/init-shrface ()
  (use-package shrface
    :if my-display-enable-shrface
    :after shr
    :config
    (progn
      (with-eval-after-load 'shr
        (require 'shrface)
        (shrface-basic)
        (shrface-trial)
        (setq shrface-href-versatile t)
        (setq shrface-toggle-bullets nil)

        ;; eww support
        (with-eval-after-load 'eww
          (add-hook 'eww-after-render-hook 'shrface-mode))

        ;; nov support
        (with-eval-after-load 'nov
          (setq nov-shr-rendering-functions
                '((img . nov-render-img)
                  (title . nov-render-title)))
          (setq nov-shr-rendering-functions
                (append nov-shr-rendering-functions
                        shr-external-rendering-functions))
          (add-hook 'nov-mode-hook 'shrface-mode))

        ;; mu4e support
        (with-eval-after-load 'mu4e
          (add-hook 'mu4e-view-mode-hook 'shrface-mode)))
      )
    ))

(defun my-display/init-shr-tag-pre-highlight ()
  (use-package shr-tag-pre-highlight
    :if my-display-enable-shrface
    :after shr
    :config
    (progn
      (add-to-list 'shr-external-rendering-functions
                   '(pre . shr-tag-pre-highlight))
      (when (version< emacs-version "26")
        (with-eval-after-load 'eww
          (advice-add 'eww-display-html :around
                      'eww-display-html--override-shr-external-rendering-functions)))
      )
    ))

(defun my-display/init-inherit-org ()
  (use-package inherit-org
    :if my-display-enable-inherit-org
    :after org
    :config
    (progn
      (with-eval-after-load 'org
        (require 'inherit-org)

        ;; info support
        (with-eval-after-load 'info
          (add-hook 'Info-mode-hook 'inherit-org-mode))

        ;; helpful support
        (with-eval-after-load 'helpful
          (add-hook 'helpful-mode-hook 'inherit-org-mode))

        ;; w3m support
        (with-eval-after-load 'w3m
          (add-hook 'w3m-fontify-before-hook 'inherit-org-w3m-headline-fontify)
          (add-hook 'w3m-fontify-after-hook 'inherit-org-mode)))
      )
    ))


;;; packages.el ends here
