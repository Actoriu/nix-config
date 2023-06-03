;;; packages.el --- Auto-completion Layer packages File for Spacemacs
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

(defconst my-auto-completion-packages
  '(
    yasnippet
    )
  )

(defun my-auto-completion/post-init-yasnippet ()
  (use-package yasnippet
    :init
    (progn
      (let* ((my-yasnippet-dir
              (configuration-layer/get-layer-local-dir 'my-auto-completion))
             (my-yasnippet-layer-snippets-dir (expand-file-name
                                               "snippets"
                                               my-yasnippet-dir)))
        (add-to-list 'yas-snippet-dirs my-yasnippet-layer-snippets-dir))

      ;; Auto add HEADER in new file
      (add-hook 'find-file-hook
                '(lambda ()
                   (when (and (buffer-file-name)
                              (not (file-exists-p (buffer-file-name)))
                              (= (point-max) 1))
                     (let ((header-snippet "HEADER")
                           (yas/fallback-behavior 'return-nil))
                       (insert header-snippet)
                       ;; if can't expand snippet, delete insert string
                       (if (not (yas/expand))
                           (delete-region (point-min) (point-max)))))))
      )
    )
  )


;;; packages.el ends here
