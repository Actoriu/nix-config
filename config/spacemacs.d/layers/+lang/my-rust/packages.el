;;; packages.el --- Rust Layer packages File for Spacemacs
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

(defconst my-rust-packages
  '(
    ob-rust
    )
  )


(defun my-rust/pre-init-ob-rust ()
  (spacemacs|use-package-add-hook org
    :post-config
    (use-package ob-rust
      :init (add-to-list 'org-babel-load-languages '(rust . t))
      )
    )
  )

(defun my-rust/init-ob-rust ())


;;; packages.el ends here
