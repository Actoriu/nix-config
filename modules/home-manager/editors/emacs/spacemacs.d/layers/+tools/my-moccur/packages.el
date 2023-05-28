;;; packages.el --- Moccur Layer packages File for Spacemacs
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

(defconst my-moccur-packages
  '(
    (color-rg :location (recipe
                         :fetcher github
                         :repo "manateelazycat/color-rg"))
    (find-define :location local)
    (find-func-extension :location local)
    )
  )

(defun my-moccur/init-color-rg ()
  (use-package color-rg))

(defun my-moccur/init-find-define ()
  (use-package find-define
    :defer t
    )
  )

(defun my-moccur/init-find-func-extension ()
  (use-package find-func-extension
    :defer t
    )
  )


;;; packages.el ends here
