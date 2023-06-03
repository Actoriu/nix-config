;;; packages.el --- ebook Layer packages File for Spacemacs
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

(defconst my-ebook-packages
  '(
    calibredb
    )
  )

(defun my-ebook/init-calibredb ()
  (use-package calibredb
    :commands (calibredb)
    :config
    (progn
      (setq calibredb-format-icons t
            calibredb-format-icons-in-terminal t))
    )
  )


;;; packages.el ends here
