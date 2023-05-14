;;; packages.el --- Direnv Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Actoriu <864149939@qq.com>
;; URL: https://github.com/Actoriu/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst my-direnv-packages
  '(
    envrc
    ))

(defun my-direnv/init-envrc ()
  (use-package envrc
    :if (executable-find "direnv")
    :hook (after-init . envrc-global-mode)
    )
  )


;;; packages.el ends here
