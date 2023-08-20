;;; packages.el --- Isolate emacs dependency environment Layer packages File for Spacemacs
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

(defconst my-env-packages
  '(
    (maple-env :location (recipe
                          :fetcher github
                          :repo "honmaple/emacs-maple-env"))
    ))

(defun my-env/init-maple-env ()
  (use-package maple-env
    :hook (after-init . maple-env-mode)
    :config
    (progn
      (with-eval-after-load 'pyvenv
        (add-hook 'pyvenv-post-activate-hooks 'maple-env-mode-on)
        (add-hook 'pyvenv-post-deactivate-hooks 'maple-env-mode-on))

      (setq maple-env:path (expand-file-name "env" spacemacs-cache-directory)
            maple-env:python-packages
            '("autoflake" "epc" "flake8" "importmagic" "isort" "ptvsd" "yapf")
            maple-env:golang-packages nil
            maple-env:npm-packages nil)
      )
    )
  )


;;; packages.el ends here
