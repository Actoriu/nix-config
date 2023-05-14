;;; funcs.el --- Org functions File for Spacemacs
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

;;; org mode for billing
(defun get-year-and-month ()
  (list (format-time-string "%Y年") (format-time-string "%m月")))

(defun find-month-tree ()
  (let* ((path (get-year-and-month))
         (level 1)
         end)
    (unless (derived-mode-p 'org-mode)
      (error "Target buffer \"%s\" should be in Org mode" (current-buffer)))
    (goto-char (point-min)) ;;移动到 buffer 的开始位置
    ;; 先定位表示年份的 headline，再定位表示月份的 headline
    (dolist (heading path)
      (let ((re (format org-complex-heading-regexp-format
                        (regexp-quote heading)))
            (cnt 0))
        (if (re-search-forward re end t)
            (goto-char (point-at-bol)) ;;如果找到了 headline 就移动到对应的位置
          (progn                       ;;否则就新建一个 headline
            (or (bolp) (insert "\n"))
            (if (/= (point) (point-min)) (org-end-of-subtree t t))
            (insert (make-string level ?*) " " heading "\n"))))
      (setq level (1+ level))
      (setq end (save-excursion (org-end-of-subtree t t))))
    (org-end-of-subtree)))

;;; password
(defun random-alphanum ()
  (let* ((charset "abcdefghijklmnopqrstuvwxyz0123456789")
         (x (random 36)))
    (char-to-string (elt charset x))))

(defun create-password ()
  (let ((value ""))
    (dotimes (number 16 value)
      (setq value (concat value (random-alphanum))))))

(defun get-or-create-password ()
  (setq password (read-string "Password: "))
  (if (string= password "")
      (create-password)
    password))

;;; protocol
(defun org-capture-template-goto-link ()
  (org-capture-put :target (list 'file+headline
                                 (nth 1 (org-capture-get :target))
                                 (org-capture-get :annotation)))
  (org-capture-put-target-region-and-position)
  (widen)
  (let ((hd (nth 2 (org-capture-get :target))))
    (goto-char (point-min))
    (if (re-search-forward
         (format org-complex-heading-regexp-format (regexp-quote hd)) nil t)
        (org-end-of-subtree)
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert "* " hd "\n"))))

;;; ox-html
(defun eh-org-wash-text (text backend info)
  "导出 org file 时，删除中文之间不必要的空格。"
  (when (org-export-derived-backend-p backend 'html)
    (let ((regexp "[[:multibyte:]]")
          (string text))
      ;; org-mode 默认将一个换行符转换为空格，但中文不需要这个空格，删除。
      (setq string
            (replace-regexp-in-string
             (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp)
             "\\1\\2" string))
      ;; 删除粗体之后的空格
      (dolist (str '("</b>" "</code>" "</del>" "</i>"))
        (setq string
              (replace-regexp-in-string
               (format "\\(%s\\)\\(%s\\)[ ]+\\(%s\\)" regexp str regexp)
               "\\1\\2\\3" string)))
      ;; 删除粗体之前的空格
      (dolist (str '("<b>" "<code>" "<del>" "<i>" "<span class=\"underline\">"))
        (setq string
              (replace-regexp-in-string
               (format "\\(%s\\)[ ]+\\(%s\\)\\(%s\\)" regexp str regexp)
               "\\1\\2\\3" string)))
      string)))

;;; ox-latex
;; Originally taken from Bruno Tavernier: http://thread.gmane.org/gmane.emacs.orgmode/31150/focus=31432
;; but adapted to use latexmk 4.20 or higher.
(defun my-auto-tex-cmd (backend)
  "When exporting from .org with latex, automatically run latex,
     pdflatex, or xelatex as appropriate, using latexmk."
  (let ((texcmd))
    ;; default command: oldstyle latex via dvi
    (if (executable-find "latexmk")
        (setq texcmd "latexmk -dvi -pdfps %f"))
    ;; pdflatex -> .pdf
    (if (and (executable-find "latexmk")
             (string-match "LATEX_CMD: pdflatex" (buffer-string)))
        (setq texcmd "latexmk -pdflatex='pdflatex -file-line-error -halt-on-error -interaction=nonstopmode --shell-escape -src-specials -synctex=1 %O %S' -pdf -f %f"))
    ;; xelatex -> .pdf
    (if (and (executable-find "latexmk")
             (string-match "LATEX_CMD: xelatex" (buffer-string)))
        (setq texcmd "latexmk -xelatex='xelatex -file-line-error -halt-on-error -interaction=nonstopmode --shell-escape -src-specials -no-pdf -synctex=1 %O %S';$xdvipdfmx='xdvipdfmx -q -E -o %D %O %S' -pdfxe -f %f"))
    ;; LaTeX compilation command
    ;; note that 'org-latex-to-pdf-process' now is renamed to 'org-latex-pdf-process'
    ;; (setq org-latex-to-pdf-process (list texcmd))
    (setq org-latex-pdf-process (list texcmd))))

;; org-export-latex-after-initial-vars-hook has been removed from org-v0.8
;; (add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-cmd)
;; (add-hook 'org-export-before-parsing-hook 'my-auto-tex-cmd)

;; Specify default packages to be included in every tex file, whether pdflatex or xelatex
;; (setq org-latex-packages-alist
;;       '(("" "graphicx" t)
;;         ("" "longtable" nil)
;;         ("" "float" nil)))

(defun my-auto-tex-parameters (backend)
  "Automatically select the tex packages to include."
  ;; default packages for ordinary latex or pdflatex export
  (setq org-latex-default-packages-alist
        '(("AUTO" "inputenc" t)
          ("T1"   "fontenc"   t)
          (""     "fixltx2e"  nil)
          (""     "wrapfig"   nil)
          (""     "soul"      t)
          (""     "textcomp"  t)
          (""     "marvosym"  t)
          (""     "wasysym"   t)
          (""     "latexsym"  t)
          (""     "amssymb"   t)
          (""     "hyperref"  nil)))

  ;; Packages to include when xelatex is used
  ;; (see https://github.com/kjhealy/latex-custom-kjh for the
  ;; non-standard ones.)
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq org-latex-default-packages-alist
            '(("" "fontspec" t)
              ("" "xunicode" t)
              ("" "url" t)
              ("" "rotating" t)
              ("" "memoir-article-styles" t)
              ("american" "babel" t)
              ("babel" "csquotes" t)
              ("" "listings" nil)
              ("" "listings-sweave-xelatex" nil)
              ("svgnames" "xcolor" t)
              ("" "soul" t)
              ("xetex, colorlinks=true, urlcolor=FireBrick, plainpages=false, pdfpagelabels, bookmarksnumbered" "hyperref" nil)
              )))

  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq org-latex-classes
            (cons '("article"
                    "\\documentclass[11pt,article,oneside]{memoir}
                     \\input{vc}
                     \\usepackage[style=authoryear-comp-ajs, abbreviate=true]{biblatex}
                     \\bibliography{socbib}"
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" . "\\subsection*{%s}")
                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                  org-latex-classes))))

;; org-export-latex-after-initial-vars-hook has been removed from org-v0.8
;; (add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-parameters)
;; (add-hook 'org-export-before-parsing-hook 'my-auto-tex-parameters)


;;; funcs.el ends here
