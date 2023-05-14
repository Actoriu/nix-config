;;; funcs.el --- Emms Layer functions File for Spacemacs
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

;; persp
(defun spacemacs//emms-persp-filter-save-buffers-function (buffer)
  "Filter for rcirc layout."
  (with-current-buffer buffer
    (eq major-mode 'emms-modes)))

(defun spacemacs//emms-buffer-to-persp ()
  "Add buffer to rcirc layout."
  (persp-add-buffer (current-buffer)
                    (persp-get-by-name emms-spacemacs-layout-name)))


;;; funcs.el ends here
