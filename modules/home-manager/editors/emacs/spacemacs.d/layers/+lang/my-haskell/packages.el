;;; packages.el --- Haskell Layer packages File for Spacemacs
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

(defconst my-haskell-packages
  '(
    dante
    )
  )

(defun my-haskell/post-init-dante ()
  (use-package dante
    :init
    (progn
      (if (and (eq haskell-completion-backend 'dante)
               (configuration-layer/layer-used-p 'syntax-checking)
               (executable-find "hlint"))
          (add-hook 'dante-mode-hook
                    '(lambda () (flycheck-add-next-checker 'haskell-dante
                                                           '(warning . haskell-hlint)))))
      )
    )
  )


;;; packages.el ends here
