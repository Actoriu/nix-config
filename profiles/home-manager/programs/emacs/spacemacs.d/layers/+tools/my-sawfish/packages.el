;;; packages.el --- Sawfish Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst my-sawfish-packages
  '(
    (sawfish :location local)
    ))

(defun my-sawfish/init-sawfish ()
  (use-package sawfish
    :defer t
    :mode (("\\(?:sawfish\\(?:/\\(?:custom\\|rc\\)\\|rc\\)\\)\\'" . sawfish-mode))
    )
  )


;;; packages.el ends here
