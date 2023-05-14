;;; packages.el --- Latex Layer packages File for Spacemacs
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

(defconst my-latex-packages
  '(
    auctex
    (cdlatex :requires auctex)
    )
  )

(defun my-latex/post-init-auctex ()
  (spacemacs|use-package-add-hook tex
    :post-config
    (progn
      ;; (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t"
      ;;                                  TeX-run-TeX nil
      ;;                                  (latex-mode doctex-mode) :help "Run XeLaTex"))

      (add-hook 'doc-view-mode-hook 'auto-revert-mode)
      (setq TeX-auto-untabify t ;; 在保存的时候自动去掉的 TAB 空白
            TeX-show-compilation t ;; 显示编译输出窗口
            TeX-save-query nil
            TeX-source-correlate-mode t
            TeX-source-correlate-method-active 'synctex)
      ;; 默认情况下，编译的时候总是假定正在编译的 tex 文件为主文件，如果去掉下面这行的注释，
      ;; 则在每次编译时都会询问哪一个 tex 文件是主文件。
      (setq-default TeX-master nil)
      (TeX-global-PDF-mode t) ;; 启用 PDF 模式
      ;; (if (string= "LaTeX" latex-build-command)
      ;;     ;; 默认使用 XeLaTeX 编译引擎
      ;;     (setq TeX-engine 'xetex))
      )
    )
  )

(defun my-latex/init-cdlatex ()
  (use-package cdlatex
    :defer t
    :after (:any org-mode LaTeX-mode)
    :hook ((LaTeX-mode . turn-on-cdlatex)
           (org-mode . turn-on-org-cdlatex))
    :config
    (setq
     cdlatex-command-alist
     '(("lim"        "Insert \\lim_{}\\limits_{}"
        "\\lim\\limits_{?}" cdlatex-position-cursor nil nil t)
       ("sin"        "Insert \\sin"
        "\\sin"       nil nil t t)
       ("cos"        "Insert \\cos"
        "\\cos"       nil nil t t)
       ("under"      "Insert \\underset{}{}"
        "\\underset{?}{}" cdlatex-position-cursor nil nil t)
       ("prod"       "Insert \\prod\\limits_{}"
        "\\prod\\limits_{?}" cdlatex-position-cursor nil nil t)
       ("txt"        "Insert \\text{}"
        "\\text{?}" cdlatex-position-cursor nil nil t)
       ("iintl"      "Insert \\iint\\limits_{}"
        "\\iint\\limits_{?}" cdlatex-position-cursor nil nil t))
     cdlatex-env-alist
     '(("cases" "\\begin{cases}\n?\n\\end{cases}\n" nil)
       ("aligned" "\\begin{aligned}\n?\n\\end{aligned}\n" nil)))
    ))


;;; packages.el ends here
