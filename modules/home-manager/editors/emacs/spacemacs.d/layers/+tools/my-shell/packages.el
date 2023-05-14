;;; packages.el --- Shell Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst my-shell-packages
  '(
    ;; bash-completion
    company
    (company-async-files :location (recipe
                                    :fetcher github
                                    :repo "CeleritasCelery/company-async-files")
                         :requires company)
    company-native-complete
    (helm-switch-shell :requires helm)
    multi-vterm
    native-complete
    vterm-toggle
    ))

;; (defun my-shell/init-bash-completion ()
;;   (use-package bash-completion
;;     :config
;;     ;; (defun my-shell-hook ()
;;     ;;   (progn
;;     ;;     (yas-minor-mode t)
;;     ;;     (if (tramp-tramp-file-p default-directory)
;;     ;;         (progn
;;     ;;           (message "Detected tramp shell!")
;;     ;;           (setq-local company-backends '((company-yankpad company-yasnippet)))
;;     ;;           (setq-local bash-completion-enabled nil))
;;     ;;       (setq-local company-backends '((company-capf company-yankpad company-yasnippet)))
;;     ;;       (setq-local company-idle-delay nil)
;;     ;;       )
;;     ;;     ))
;;     ;; (add-hook 'shell-mode-hook #'my-shell-hook)
;;     ;; (setq shell-file-name "/bin/bash")
;;     (bash-completion-setup)))

(defun my-shell/post-init-company ()
  (add-hook 'shell-mode-hook #'spacemacs//shell-setup-company))

(defun my-shell/init-company-async-files ()
  (use-package company-async-files
    :after company
    :init
    (spacemacs|add-company-backends
      :backends company-async-files
      :modes shell-mode)
    )
  )

(defun my-shell/init-company-native-complete ()
  (use-package company-native-complete
    :if (eq shell-backend 'company-native-complete)
    :config
    (progn
      (setq shell-file-name (executable-find "bash")))))

(defun my-shell/init-helm-switch-shell ()
  (use-package helm-switch-shell
    :defer t))

(defun my-shell/init-multi-vterm ()
  (use-package multi-vterm
    :after vterm))

(defun my-shell/init-native-complete ()
  (use-package native-complete
    :if (eq shell-backend 'company-native-complete)
    :custom
    (native-complete-style-regex-alist
     `(("╰─→ " . bash)
       (,(rx (or "% " (: (+ (in "0-9")) "> ") ")> ")) . zsh)
       ("[A-Z]+> " . tab)))
    :init
    (progn
      (with-eval-after-load 'shell
        (native-complete-setup-bash))
      (add-hook 'shell-mode-hook #'spacemacs//shell-setup-backend))))

(defun my-shell/init-vterm-toggle ()
  (use-package vterm-toggle
    :after vterm))


;;; packages.el ends here
