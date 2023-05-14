;;; packages.el --- Shell Scripts Layer packages File for Spacemacs
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

(defconst my-shell-scripts-packages
  '(
    elvish-mode
    ob-elvish
    ))

(defun my-shell-scripts/init-elvish-mode ()
  (use-package elvish-mode))

(defun my-shell-scripts/pre-init-ob-elvish ()
  (spacemacs|use-package-add-hook org
    :post-config
    (use-package ob-elvish
      :init (add-to-list 'org-babel-load-languages '(elvish . t))
      )
    )
  )

(defun my-shell-scripts/init-ob-elvish ())


;;; packages.el ends here
