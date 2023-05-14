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
    ;; (grammatical-edit :location (recipe
    ;;                              :fetcher github
    ;;                              :repo "manateelazycat/grammatical-edit")
    ;;                   :requires tree-sitter)
    )
  )

(defun my-tree-sitter/pre-init-tree-sitter ()
  (use-package tree-sitter
    ;; :hook
    ;; ((agda-mode
    ;;   c-mode
    ;;   c++-mode
    ;;   cmake-mode
    ;;   coffee-mode
    ;;   csharp-mode
    ;;   css-mode
    ;;   emacs-lisp-mode
    ;;   ess-r-mode
    ;;   go-mode
    ;;   haskell-mode
    ;;   groovy-mode
    ;;   html-mode
    ;;   ielm-mode
    ;;   jade-mode
    ;;   java-mode
    ;;   javascript-mode
    ;;   js-mode
    ;;   js2-mode
    ;;   js3-mode
    ;;   json-mode
    ;;   jsonc-mode
    ;;   lisp-interaction-mode
    ;;   lisp-mode
    ;;   lua-mode
    ;;   makefile-gmake-mode
    ;;   maxima-mode
    ;;   ;; minibuffer-inactive-mode
    ;;   nim-mode
    ;;   nix-mode
    ;;   php-mode
    ;;   python-mode
    ;;   qmake-mode
    ;;   qml-mode
    ;;   rjsx-mode
    ;;   ruby-mode
    ;;   rust-mode
    ;;   rustic-mode
    ;;   scala-mode
    ;;   sh-mode
    ;;   swift-mode
    ;;   typescript-mode
    ;;   ) . tree-sitter-mode)
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

;; (defun my-tree-sitter/init-grammatical-edit ()
;;   (use-package grammatical-edit
;;     :defer t
;;     :hook
;;     ((agda-mode
;;       c-mode
;;       c++-mode
;;       cmake-mode
;;       coffee-mode
;;       csharp-mode
;;       css-mode
;;       emacs-lisp-mode
;;       ess-r-mode
;;       go-mode
;;       haskell-mode
;;       groovy-mode
;;       html-mode
;;       ielm-mode
;;       jade-mode
;;       java-mode
;;       javascript-mode
;;       js-mode
;;       js2-mode
;;       js3-mode
;;       json-mode
;;       jsonc-mode
;;       lisp-interaction-mode
;;       lisp-mode
;;       lua-mode
;;       makefile-gmake-mode
;;       maxima-mode
;;       ;; minibuffer-inactive-mode
;;       nim-mode
;;       nix-mode
;;       php-mode
;;       python-mode
;;       qmake-mode
;;       qml-mode
;;       rjsx-mode
;;       ruby-mode
;;       rust-mode
;;       rustic-mode
;;       scala-mode
;;       sh-mode
;;       swift-mode
;;       typescript-mode
;;       ) . grammatical-edit-mode)
;;     ;; :bind (:map grammatical-edit-mode-map
;;     ;;             ("(" . grammatical-edit-open-round)
;;     ;;             ("[" . grammatical-edit-open-bracket)
;;     ;;             ("{" . grammatical-edit-open-curly)
;;     ;;             (")" . grammatical-edit-close-round)
;;     ;;             ("]" . grammatical-edit-close-bracket)
;;     ;;             ("}" . grammatical-edit-close-curly)
;;     ;;             ("=" . grammatical-edit-equal)

;;     ;;             ("%" . grammatical-edit-match-paren)
;;     ;;             ("\"" . grammatical-edit-double-quote)

;;     ;;             ("SPC" . grammatical-edit-space)
;;     ;;             ("RET" . grammatical-edit-newline)

;;     ;;             ("M-o" . grammatical-edit-backward-delete)
;;     ;;             ("C-d" . grammatical-edit-forward-delete)
;;     ;;             ("C-k" . grammatical-edit-kill)

;;     ;;             ("M-\"" . grammatical-edit-wrap-double-quote)
;;     ;;             ("M-[" . grammatical-edit-wrap-bracket)
;;     ;;             ("M-{" . grammatical-edit-wrap-curly)
;;     ;;             ("M-(" . grammatical-edit-wrap-round)
;;     ;;             ("M-)" . grammatical-edit-unwrap)

;;     ;;             ("M-p" . grammatical-edit-jump-right)
;;     ;;             ("M-n" . grammatical-edit-jump-left)
;;     ;;             ("M-:" . grammatical-edit-jump-out-pair-and-newline))
;;     )
;;   )


;;; packages.el ends here
