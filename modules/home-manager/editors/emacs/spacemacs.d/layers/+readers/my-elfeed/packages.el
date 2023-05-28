;;; packages.el --- elfeed Layer packages File for Spacemacs
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

(defconst my-elfeed-packages
  '(
    elfeed
    elfeed-org
    )
  )

(defun my-elfeed/post-init-elfeed ()
  (use-package elfeed
    :init
    (progn
      (setq elfeed-db-directory (expand-file-name
                                 "elfeed/db"
                                 spacemacs-cache-directory)
            elfeed-enclosure-default-dir (expand-file-name
                                          "elfeed/enclosures"
                                          spacemacs-cache-directory)
            elfeed-search-filter "@1-week-ago +unread")
      )
    )
  )

(defun my-elfeed/pre-init-elfeed-org ()
  (use-package elfeed-org
    :init
    (progn
      (let* ((my-elfeed-dir
              (configuration-layer/get-layer-local-dir 'my-elfeed))
             (my-elfeed-layer-private-dir (file-name-as-directory
                                           (expand-file-name
                                            "elfeed"
                                            my-elfeed-dir)))
             (elfeed-org-private-files (expand-file-name
                                        "elfeed.org"
                                        my-elfeed-layer-private-dir)))
        (setq rmh-elfeed-org-files (list elfeed-org-private-files)))
      )
    )
  )


;;; packages.el ends here
