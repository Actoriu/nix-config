;;; packages.el --- Scheme Layer packages File for Spacemacs
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

(defconst my-scheme-packages
  '(
    (flycheck-guile :toggle (memq 'guile scheme-implementations)
                    :requires flycheck)
    geiser
    geiser-guile
    (guix :toggle (executable-find "guix"))
    macrostep-geiser
    )
  )

(defun my-scheme/init-flycheck-guile ()
  (use-package flycheck-guile
    :if (and (configuration-layer/package-used-p 'geiser-guile)
             (configuration-layer/package-used-p 'flycheck))
    :after geiser))

(defun my-scheme/post-init-geiser ()
  (use-package geiser
    ;; :hook (scheme-mode . geiser-mode--maybe-activate)
    :config
    (progn
      (setq geiser-repl-history-filename (concat spacemacs-cache-directory "geiser-history"))
      )))

(defun my-scheme/post-init-geiser-guile ()
  (use-package geiser-guile
    :if (and (configuration-layer/package-used-p 'guix)
             (executable-find "guix"))
    :config
    (add-to-list 'geiser-guile-load-path
                 (expand-file-name "~/.config/guix/current/share/guile/site/3.0"))
    ))

(defun my-scheme/init-guix ()
  (use-package guix
    :if (and (configuration-layer/package-used-p 'geiser-guile)
             (executable-find "guix"))
    :defer t
    :hook (scheme-mode . guix-devel-mode)))

(defun my-scheme/init-macrostep-geiser ()
  (use-package macrostep-geiser
    :hook (geiser-mode . macrostep-geiser-setup)
    :hook (geiser-repl-mode . macrostep-geiser-setup)
    :after geiser
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'scheme-mode
        "mm" 'macrostep-expand
        "mM" 'macrostep-geiser-expand-all)
      (spacemacs/declare-prefix-for-mode 'geiser-repl-mode "mm" "macroexpansion")
      (spacemacs/set-leader-keys-for-major-mode 'geiser-repl-mode
        "mm" 'macrostep-expand
        "mM" 'macrostep-geiser-expand-all)
      )))


;;; packages.el ends here
