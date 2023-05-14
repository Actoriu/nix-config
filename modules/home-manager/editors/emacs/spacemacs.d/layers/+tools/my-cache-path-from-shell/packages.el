;;; packages.el --- cache-path-from-shell Layer packages File for Spacemacs
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

(defconst my-cache-path-from-shell-packages
  '(
    (cache-path-from-shell :location (recipe
                                      :fetcher github
                                      :repo "manateelazycat/cache-path-from-shell")
                           :requires exec-path-from-shell)
    ))

(defun my-cache-path-from-shell/init-cache-path-from-shell ()
  (use-package cache-path-from-shell
    :if (featurep 'exec-path-from-shell)))


;;; packages.el ends here
