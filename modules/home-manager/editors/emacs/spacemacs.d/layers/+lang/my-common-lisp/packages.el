;;; packages.el --- Common Lisp Layer packages File for Spacemacs
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

(defconst my-common-lisp-packages
  '(
    slime
    )
  )

(defun my-common-lisp/post-init-slime ()
  (use-package slime
    :if common-lisp-enable-slime
    :config
    (progn
      (cond
       ;; (common-lisp-enable-local-hyperspec-root
       ;;  (let* ((my-common-lisp-hyperspec-dir
       ;;          (configuration-layer/get-layer-local-dir 'my-common-lisp))
       ;;         (my-common-lisp-layer-hyperspec-root-dir (file-name-as-directory
       ;;                                                   (expand-file-name
       ;;                                                    "HyperSpec-7-0/HyperSpec"
       ;;                                                    my-common-lisp-hyperspec-dir)))
       ;;         (my-common-lisp-layer-hyperspec-data-dir (file-name-as-directory
       ;;                                                   (expand-file-name
       ;;                                                    "Data"
       ;;                                                    my-common-lisp-layer-hyperspec-root-dir))))
       ;;    (setq common-lisp-hyperspec-root
       ;;          (concat "file://" (expand-file-name my-common-lisp-layer-hyperspec-root-dir))
       ;;          ;; common-lisp-hyperspec-symbol-table
       ;;          ;; (expand-file-name "Map_Sym.txt"
       ;;          ;;                   my-common-lisp-layer-hyperspec-data-dir)
       ;;          ;; common-lisp-hyperspec-issuex-table
       ;;          ;; (expand-file-name "Map_IssX.txt"
       ;;          ;;                   my-common-lisp-layer-hyperspec-data-dir)
       ;;          ))
       ;;  )
       ((spacemacs/system-is-mac)
        (cond ((and (executable-find "sbcl")
                    (not (executable-find "dx86cl64")))
               (setq inferior-lisp-program "sbcl -K utf-8")
               (setq slime-lisp-implementations
                     `((sbcl ("sbcl") :coding-system utf-8-unix)
                       (clozure_cl ("dx86cl64") :coding-system utf-8-unix)))
               )
              ((and (not (executable-find "sbcl"))
                    (executable-find "dx86cl64"))
               (setq inferior-lisp-program "dx86cl64 -K utf-8")
               (setq slime-lisp-implementations
                     `((clozure_cl ("dx86cl64") :coding-system utf-8-unix)
                       (sbcl ("sbcl") :coding-system utf-8-unix)))
               )
              )
        )
       ((spacemacs/system-is-linux)
        (cond ((and (executable-find "sbcl")
                    (not (executable-find "ccl")))
               (setq inferior-lisp-program "sbcl -K utf-8")
               (setq slime-lisp-implementations
                     `((sbcl ("sbcl") :coding-system utf-8-unix)
                       (clozure_cl ("ccl") :coding-system utf-8-unix)))
               )
              ((and (not (executable-find "sbcl"))
                    (executable-find "ccl"))
               (setq inferior-lisp-program "ccl -K utf-8")
               (setq slime-lisp-implementations
                     `((clozure_cl ("ccl") :coding-system utf-8-unix)
                       (sbcl ("sbcl") :coding-system utf-8-unix)))
               )
              )
        )
       ((spacemacs/system-is-mswindows)
        (cond ((and (executable-find "sbcl.exe")
                    (not (or (executable-find "wx86cl64.exe")
                             (executable-find "wx86cl.exe"))))
               (setq inferior-lisp-program "sbcl.exe -K utf-8")
               (setq slime-lisp-implementations
                     `((sbcl ("sbcl.exe") :coding-system utf-8-unix)
                       (clozure_cl '((if (getenv "PROGRAMW6432")
                                         "wx86cl64.exe" "wx86cl.exe"))
                                   :coding-system utf-8-unix)))
               )
              ((and (not (executable-find "sbcl.exe"))
                    (or (executable-find "wx86cl64.exe")
                        (executable-find "wx86cl.exe")))
               (setq inferior-lisp-program "wx86cl64.exe -K utf-8")
               (setq slime-lisp-implementations
                     `((clozure_cl '((if (getenv "PROGRAMW6432")
                                         "wx86cl64.exe" "wx86cl.exe"))
                                   :coding-system utf-8-unix)
                       (sbcl ("sbcl.exe") :coding-system utf-8-unix)))
               )
              )
        )
       )
      )
    )
  )


;;; packages.el ends here
