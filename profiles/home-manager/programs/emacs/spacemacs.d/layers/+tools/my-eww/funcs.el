;;; funcs.el --- Eww Layer functions File for Spacemacs
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

(defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
  "Disable preview backupground colors in eww."
  (unless eww-disable-colorize
    (funcall orig start end fg)))

(defun eww-enable-color ()
  "Enable preview with font colors in eww."
  (interactive)
  (setq-local eww-disable-colorize nil)
  (eww-reload)
  (message "Now you can preview with backupground colors in eww."))

(defun eww-disable-color ()
  "Disable preview with font colors in eww."
  (interactive)
  (setq-local eww-disable-colorize t)
  (eww-reload)
  (message "Now you can't preview with backupground colors in eww."))

(defun eww-enable-images ()
  "Enable preview with pictures in eww."
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image)
  (eww-reload)
  (message "Now you can preview with pictures in eww."))

(defun eww-disable-images ()
  "Disable preview with pictures in eww."
  (interactive)
  (setq-local shr-put-image-function 'shr-put-image-alt)
  (eww-reload)
  (message "Now you can't preview with pictures in eww."))

(defun shr-put-image-alt (spec alt &optional flags)
  "Disable preview with pictures in eww."
  (insert alt))

(defun eww-mode-hook--disable-image ()
  "Disable preview with pictures in eww."
  (setq-local shr-put-image-function 'shr-put-image-alt))


;;; funcs.el ends here
