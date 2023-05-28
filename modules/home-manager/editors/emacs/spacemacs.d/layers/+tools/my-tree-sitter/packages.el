;;; packages.el --- Tree-sitter Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst my-tree-sitter-packages
  '(
    tree-sitter
    )
  )

(defun my-tree-sitter/pre-init-tree-sitter ()
  (use-package tree-sitter
    :config
    (progn
      ;; (add-to-list 'tree-sitter-load-path
      ;;              (expand-file-name
      ;;               "tree-sitter-langs/bin"
      ;;               (configuration-layer/get-layer-local-dir 'my-tree-sitter)))
      (tree-sitter-load 'commonlisp "commonlisp")
      (add-to-list 'tree-sitter-major-mode-language-alist '(lisp-mode . commonlisp))
      (tree-sitter-load 'elisp "elisp")
      (add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))
      (add-to-list 'tree-sitter-major-mode-language-alist '(inferior-emacs-lisp-mode . elisp))
      )
    )
  )


;;; packages.el ends here
