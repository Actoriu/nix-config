;;; layers.el --- Shell Layer declarations File for Spacemacs
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

(when (and (boundp 'shell-backend)
           (eq shell-backend 'lsp))
  (configuration-layer/declare-layer-dependencies '(lsp)))


;;; layers.el ends here
