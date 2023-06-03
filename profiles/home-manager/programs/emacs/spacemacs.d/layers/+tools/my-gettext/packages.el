;;; packages.el --- Gettext Layer packages File for Spacemacs
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

(defconst my-gettext-packages
  '(
    po-mode
    (mo-mode :location local)
    ))

(defun my-gettext/init-po-mode ()
  (use-package po-mode
    :defer t
    ;; :mode ("\\.po\\'" . po-mode)
    ;; :init
    ;; (progn
    ;;   (autoload 'po-find-file-coding-system "po-compat")
    ;;   (modify-coding-system-alist 'file "\\.po\\'\\|\\.po\\."
    ;;                               'po-find-file-coding-system)
    ;;   )
    :config
    (progn
      (add-hook 'po-subedit-mode-hook '(lambda () (longlines-mode 1)))
      (add-hook 'po-subedit-exit-hook '(lambda () (longlines-mode 0)))

      ;;TODO the keybindings do not seem to work, despite it being similarly
      ;;structured as the Markdown mode.

      ;; Declare prefixes and bind keys
      (dolist (prefix '(("ml" . "language")
                        ("me" . "edit")
                        ("ms" . "search")
                        ("mc" . "command")))
        (spacemacs/declare-prefix-for-mode
          'po-mode (car prefix) (cdr prefix)))

      (spacemacs/set-leader-keys-for-major-mode 'po-mode
        ;;command
        "e" 'po-edit-msgstr
        "cV" 'po-validate))
    )
  )

(defun my-gettext/init-mo-mode ()
  (use-package mo-mode
    :defer t
    :mode ("\\.g?mo\\'" . mo-mode)
    :init
    (modify-coding-system-alist 'file "\\.g?mo\\'" 'raw-text-unix))
  )


;;; packages.el ends here
