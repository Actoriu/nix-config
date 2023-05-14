;;; packages.el --- DAP Layer packages File for Spacemacs
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

(defconst my-dap-packages
  '(
    dap-mode
    ))

(defun my-dap/pre-init-dap-mode ()
  (spacemacs|use-package-add-hook dap-mode
    :post-init
    (setq dap-breakpoints-file (concat spacemacs-cache-directory ".dap-breakpoints")
          dap-utils-extension-path (expand-file-name
                                    (locate-user-emacs-file (f-join ".cache" "dap" "extension"))))))


;;; packages.el ends here
