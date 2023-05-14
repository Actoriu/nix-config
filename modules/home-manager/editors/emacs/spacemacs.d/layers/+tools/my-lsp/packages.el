;;; packages.el --- Language Server Protocol Layer packages File for Spacemacs
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

(defconst my-lsp-packages
  '(
    lsp-mode
    ))

(defun my-lsp/pre-init-lsp-mode ()
  (spacemacs|use-package-add-hook lsp-mode
    :post-init
    (setq lsp-session-file (concat spacemacs-cache-directory ".lsp-session-v1"))))


;;; packages.el ends here
