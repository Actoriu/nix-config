;;; packages.el --- Org Layer packages File for Spacemacs
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

(defconst my-org-packages
  '(
    (org :location elpa :min-version "9.5")
    (org-protocol :location built-in)
    (org-tempo :location built-in)
    (org-crypt :location built-in
               :toggle org-enable-encryption)
    (ox :location built-in)
    (ox-html :location built-in)
    (ox-latex :location built-in
              :toggle org-enable-latex-export)
    (ox-bibtex-chinese :requires ox ox-bibtex
                       :toggle org-enable-bibtex-export)
    ;; (valign :location (recipe :fetcher github :repo "casouri/valign")
    ;;         :toggle org-enable-valign)
    )
  )

(defun my-org/pre-init-org ()
  (use-package org
    :init
    (progn
      (setq my-org-layer-private-dir
            (configuration-layer/get-layer-local-dir 'my-org)
            my-private-org-directory (file-name-as-directory
                                      (expand-file-name
                                       "org"
                                       my-org-layer-private-dir))
            my-org-layer-private-notes-dir (file-name-as-directory
                                            (expand-file-name
                                             "notes"
                                             my-private-org-directory))
            my-org-layer-private-blog-dir (file-name-as-directory
                                           (expand-file-name
                                            "blog"
                                            my-private-org-directory))
            org-default-notes-file (expand-file-name
                                    "gtd.org"
                                    my-org-layer-private-notes-dir)
            ;; define the refile targets
            org-agenda-file-note (expand-file-name
                                  "notes.org"
                                  my-org-layer-private-notes-dir)
            org-agenda-file-inbox (expand-file-name
                                   "inbox.org"
                                   my-org-layer-private-notes-dir)
            org-agenda-file-billing (expand-file-name
                                     "billing.org"
                                     my-org-layer-private-notes-dir)
            org-agenda-file-contacts (expand-file-name
                                      "contacts.org"
                                      my-org-layer-private-notes-dir)
            org-agenda-file-gtd (expand-file-name
                                 "gtd.org"
                                 my-org-layer-private-notes-dir)
            org-agenda-file-journal (expand-file-name
                                     "journal.org"
                                     my-org-layer-private-notes-dir)
            org-agenda-file-code-snippet (expand-file-name
                                          "snippet.org"
                                          my-org-layer-private-notes-dir)
            org-agenda-file-code-password (expand-file-name
                                           "passwords.org"
                                           my-org-layer-private-notes-dir)
            org-agenda-file-protocol (expand-file-name
                                      "bookmarks.org"
                                      my-org-layer-private-notes-dir)
            org-agenda-files (list my-org-layer-private-notes-dir))
      ;; Specify default packages to be included in every tex file, whether pdflatex or xelatex
      (setq org-latex-packages-alist
            '(("" "graphicx" t)
              ("" "longtable" nil)
              ("" "float" nil)))
      (setq org-capture-templates nil)
      (add-to-list 'org-capture-templates
                   '("r" "阅读任务" entry
                     (file+olp org-agenda-file-gtd "正在阅读" "书名")
                     "* TODO %^{书名}\n%u\n%a\n"
                     :clock-in t :clock-resume t))
      (add-to-list 'org-capture-templates
                   '("w" "工作任务" entry
                     (file+headline org-agenda-file-gtd "工作")
                     "* TODO %^{任务名}\n%u\n%a\n"
                     :clock-in t :clock-resume t))
      (add-to-list 'org-capture-templates
                   '("j" "日志" entry
                     (file+olp+datetree org-agenda-file-journal)
                     "* %U - %^{heading} %^g\n %?\n"))
      (add-to-list 'org-capture-templates
                   '("i" "灵感" entry
                     (file org-agenda-file-inbox)
                     "* %U - %^{heading} %^g\n %?\n"))
      (add-to-list 'org-capture-templates
                   '("n" "笔记" entry
                     (file org-agenda-file-note)
                     "* %^{heading} %t %^g\n %?\n"))
      (add-to-list 'org-capture-templates
                   '("b" "记账" plain
                     (file+function org-agenda-file-billing find-month-tree)
                     " | %U | %^{类别} | %^{描述} | %^{金额} |"
                     :kill-buffer t))
      (add-to-list 'org-capture-templates
                   '("c" "联系人" entry
                     (file org-agenda-file-contacts)
                     "* %^{姓名} %^{手机号}p %^{邮箱}p %^{住址}p\n\n %?"
                     :empty-lines 1))
      (add-to-list 'org-capture-templates
                   '("p" "密码管理" entry
                     (file org-agenda-file-code-password)
                     "* %U - %^{title} %^G\n\n - 用户名: %^{用户名}\n - 密码: %(get-or-create-password)"
                     :empty-lines 1 :kill-buffer t))
      (add-to-list 'org-capture-templates
                   '("l" "书签" plain
                     (file+function org-agenda-file-protocol org-capture-template-goto-link)
                     "\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n #+BEGIN_EXAMPLE\n%:initial#+END_EXAMPLE" :empty-lines 1))
      )
    :config
    (progn
      ;; 折叠时不再显示「...」, 换个你喜欢的符号
      (setq org-ellipsis "▾"))
    )
  )

(defun my-org/init-org-protocol ()
  (use-package org-protocol
    :after (org server)))

;;; Org 9.2 comes with a new template expansion mechanism,
;;; combining org-insert-structure-template bound to C-c C-, .
;;; If you prefer using previous patterns, e.g. <s ,
;;; you can activate them again by requiring Org Tempo library:
;;; (require 'org-tempo) or add it to org-modules .
(defun my-org/init-org-tempo ()
  (use-package org-tempo
    :after org))

(defun my-org/init-org-crypt ()
  (use-package org-crypt
    :if org-enable-encryption
    :config
    (progn
      ;; Disable `auto-save-mode' for org-mode buffer prior to decrypting an entry.
      (setq org-crypt-disable-auto-save t)
      ;; Auto encrypt when save file
      (org-crypt-use-before-save-magic)
      ;; Encrypt with tagname: `secret'
      (setq org-crypt-tag-matcher "secret")
      ;; Prevent the `secret' tag inherit by child
      ;; (The child item still will be encrypt)
      (setq org-tags-exclude-from-inheritance (quote ("secret")))
      ;; Use my own password to encrypt
      (setq org-crypt-key nil)
      )
    )
  )

(defun my-org/init-ox ()
  (use-package ox
    :commands (org-export-as org-export-to-file)
    :init
    (progn
      (add-hook 'org-export-before-parsing-hook 'my-auto-tex-cmd)
      (add-hook 'org-export-before-parsing-hook 'my-auto-tex-parameters)
      )
    :config
    (progn
      ;; Export language
      (setq org-export-default-language "zh-CN"
            ;; org默认使用"_下标"来定义一个下标，使用"^上标"定义一个上标，
            ;; 但这种方式在中文环境中与下划线冲突。
            ;; 这里强制使用"_{下标}"来定义一个下标。"^{上标}"来定义一个上标。
            org-export-with-sub-superscripts '{}
            org-use-sub-superscripts '{})
      )
    )
  )

(defun my-org/init-ox-html ()
  (use-package ox-html
    :config
    (progn
      (setq org-html-coding-system 'utf-8
            org-html-head-include-default-style t
            org-html-head-include-scripts t)
      (add-hook 'org-export-filter-headline-functions #'eh-org-wash-text)
      (add-hook 'org-export-filter-paragraph-functions #'eh-org-wash-text)
      )
    )
  )

(defun my-org/init-ox-latex ()
  (use-package ox-latex
    :if org-enable-latex-export
    :init
    (progn
      ;;org-mode source code setup in exporting to latex
      (add-to-list 'org-latex-listings '("" "listings"))
      (add-to-list 'org-latex-listings '("" "color"))

      (add-to-list 'org-latex-packages-alist
                   '("" "xcolor" t))
      (add-to-list 'org-latex-packages-alist
                   '("" "listings" t))
      (add-to-list 'org-latex-packages-alist
                   '("" "fontspec" t))
      (add-to-list 'org-latex-packages-alist
                   '("" "indentfirst" t))
      (add-to-list 'org-latex-packages-alist
                   '("" "xunicode" t))
      (add-to-list 'org-latex-packages-alist
                   '("" "geometry"))
      (add-to-list 'org-latex-packages-alist
                   '("" "float"))
      (add-to-list 'org-latex-packages-alist
                   '("" "longtable"))
      (add-to-list 'org-latex-packages-alist
                   '("" "tikz"))
      (add-to-list 'org-latex-packages-alist
                   '("" "fancyhdr"))
      (add-to-list 'org-latex-packages-alist
                   '("" "textcomp"))
      (add-to-list 'org-latex-packages-alist
                   '("" "amsmath"))
      (add-to-list 'org-latex-packages-alist
                   '("" "tabularx" t))
      (add-to-list 'org-latex-packages-alist
                   '("" "booktabs" t))
      (add-to-list 'org-latex-packages-alist
                   '("" "grffile" t))
      (add-to-list 'org-latex-packages-alist
                   '("" "wrapfig" t))
      (add-to-list 'org-latex-packages-alist
                   '("normalem" "ulem" t))
      (add-to-list 'org-latex-packages-alist
                   '("" "amssymb" t))
      (add-to-list 'org-latex-packages-alist
                   '("" "capt-of" t))
      (add-to-list 'org-latex-packages-alist
                   '("figuresright" "rotating" t))
      (add-to-list 'org-latex-packages-alist
                   '("Lenny" "fncychap" t))

      (add-to-list 'org-latex-classes
                   '("cn-org-book"
                     "\\documentclass[UTF-8,oneside,A4paper,12pt]{book}
                     \\usepackage[slantfont, boldfont]{xeCJK}
                     \\usepackage{xltxtra}
                     % chapter set
                     \\usepackage{titlesec}
                     \\usepackage{minted}
                     [NO-DEFAULT-PACKAGES]
                     [PACKAGES]
                     \\setCJKmainfont{Noto Serif CJK SC} % 设置缺省中文字体
                     \\setCJKsansfont{Noto Sans CJK SC}
                     \\setCJKmonofont{Noto Sans Mono CJK SC}
                     \\setmainfont{Noto Serif} % 英文衬线字体
                     \\setsansfont{Noto Sans} % 英文无衬线字体
                     \\setmonofont{Noto Sans Mono}
                     %\\setmainfont{Noto Serif CJK SC} % 设置缺省中文字体
                     %\\setsansfont{Noto Sans CJK SC}
                     %\\setmonofont{Noto Sans Mono CJK SC}
                     %如果没有它，会有一些 tex 特殊字符无法正常使用，比如连字符。
                     \\defaultfontfeatures{Mapping=tex-text}
                     % [FIXME] ox-latex 的設計不良導致 hypersetup 必須在這裡插入
                     \\usepackage{hyperref}
                     \\hypersetup{
                     colorlinks=true, %把紅框框移掉改用字體顏色不同來顯示連結
                     linkcolor=[rgb]{0,0.37,0.53},
                     citecolor=[rgb]{0,0.47,0.68},
                     filecolor=[rgb]{0,0.37,0.53},
                     urlcolor=[rgb]{0,0.37,0.53},
                     linktoc=all,}
                     % 中文断行
                     \\XeTeXlinebreaklocale \"zh\"
                     \\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt
                     % 代码设置
                     \\lstset{numbers=left,
                     numberstyle= \\tiny,
                     keywordstyle= \\color{ blue!70},
                     commentstyle=\\color{red!50!green!50!blue!50},
                     frame=shadowbox,
                     breaklines=true,
                     rulesepcolor= \\color{ red!20!green!20!blue!20}
                     }
                     [EXTRA]
                     "
                     ("\\chapter{%s}" . "\\chapter*{%s}")
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
      (add-to-list 'org-latex-classes
                   '("cn-org-article"
                     "\\documentclass[UTF-8,oneside,A4paper,12pt]{article}
                     \\usepackage[CJKspace]{xeCJK}
                     \\usepackage{xltxtra}
                     % chapter set
                     \\usepackage{titlesec}
                     \\usepackage{minted}
                     [NO-DEFAULT-PACKAGES]
                     [PACKAGES]
                     \\parindent 2em
                     \\setCJKmainfont{Noto Serif CJK SC} % 设置缺省中文字体
                     \\setCJKsansfont{Noto Sans CJK SC}
                     \\setCJKmonofont{Noto Sans Mono CJK SC}
                     \\setmainfont{Noto Serif} % 英文衬线字体
                     \\setsansfont{Noto Sans} % 英文无衬线字体
                     \\setmonofont{Noto Sans Mono}
                     %\\setmainfont{Noto Serif CJK SC} % 设置缺省中文字体
                     %\\setsansfont{Noto Sans CJK SC}
                     %\\setmonofont{Noto Sans Mono CJK SC}
                     %如果没有它，会有一些 tex 特殊字符无法正常使用，比如连字符。
                     \\defaultfontfeatures{Mapping=tex-text}
                     % [FIXME] ox-latex 的設計不良導致 hypersetup 必須在這裡插入
                     \\usepackage{hyperref}
                     \\hypersetup{
                     colorlinks=true, %把紅框框移掉改用字體顏色不同來顯示連結
                     linkcolor=[rgb]{0,0.37,0.53},
                     citecolor=[rgb]{0,0.47,0.68},
                     filecolor=[rgb]{0,0.37,0.53},
                     urlcolor=[rgb]{0,0.37,0.53},
                     linktoc=all,}
                     % 中文断行
                     \\XeTeXlinebreaklocale \"zh\"
                     \\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt
                     % 代码设置
                     \\lstset{numbers=left,
                     numberstyle= \\tiny,
                     keywordstyle= \\color{ blue!70},
                     commentstyle=\\color{red!50!green!50!blue!50},
                     frame=shadowbox,
                     breaklines=true,
                     rulesepcolor= \\color{ red!20!green!20!blue!20}
                     }
                     [EXTRA]
                     "
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
      (add-to-list 'org-latex-classes
                   '("cn-org-beamer"
                     "\\documentclass[UTF-8,oneside,A4paper,12pt]{beamer}
                     \\usepackage[slantfont, boldfont]{xeCJK}
                     \\usepackage{xltxtra}
                     % chapter set
                     \\usepackage{minted}
                     \\usepackage[none]{hyphenat}
                     \\usepackage[abs]{overpic}
                     [NO-DEFAULT-PACKAGES]
                     [PACKAGES]
                     \\setCJKmainfont{Noto Serif CJK SC} % 设置缺省中文字体
                     \\setCJKsansfont{Noto Sans CJK SC}
                     \\setCJKmonofont{Noto Sans Mono CJK SC}
                     \\setmainfont{Noto Serif} % 英文衬线字体
                     \\setsansfont{Noto Sans} % 英文无衬线字体
                     \\setmonofont{Noto Sans Mono}
                     %\\setmainfont{Noto Serif CJK SC} % 设置缺省中文字体
                     %\\setsansfont{Noto Sans CJK SC}
                     %\\setmonofont{Noto Sans Mono CJK SC}
                     %如果没有它，会有一些 tex 特殊字符无法正常使用，比如连字符。
                     \\defaultfontfeatures{Mapping=tex-text}
                     % [FIXME] ox-latex 的設計不良導致 hypersetup 必須在這裡插入
                     \\usepackage{hyperref}
                     \\hypersetup{
                     colorlinks=true, %把紅框框移掉改用字體顏色不同來顯示連結
                     linkcolor=[rgb]{0,0.37,0.53},
                     citecolor=[rgb]{0,0.47,0.68},
                     filecolor=[rgb]{0,0.37,0.53},
                     urlcolor=[rgb]{0,0.37,0.53},
                     linktoc=all,}
                     % 中文断行
                     \\XeTeXlinebreaklocale \"zh\"
                     \\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt
                     % 代码设置
                     \\lstset{numbers=left,
                     numberstyle= \\tiny,
                     keywordstyle= \\color{ blue!70},
                     commentstyle=\\color{red!50!green!50!blue!50},
                     frame=shadowbox,
                     breaklines=true,
                     rulesepcolor= \\color{ red!20!green!20!blue!20}
                     }
                     [EXTRA]
                     "
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
      )
    :config
    (progn
      ;; 不要在latex输出文件中插入\maketitle
      (setq org-latex-title-command ""
            org-latex-date-format "%F"
            ;; 默认支持中文
            ;; org-latex-create-formula-image-program 'imagemagick
            ;; 速度较快，但默认不支持中文
            org-latex-create-formula-image-program 'dvipng
            org-format-latex-options
            (plist-put org-format-latex-options :scale 2.5)
            org-format-latex-options
            (plist-put org-format-latex-options :html-scale 2.5)
            )
      (setq org-latex-compiler "xelatex")
      (cond ((executable-find "biber")
             (setq org-latex-bib-compiler "biber")))
      ;; (setq org-latex-pdf-process
      ;;       '("%latex -interaction nonstopmode -output-directory %o %f"
      ;;         "%bib %b"
      ;;         "%latex -interaction nonstopmode -output-directory %o %f"
      ;;         "%latex -interaction nonstopmode -output-directory %o %f")
      ;;       org-latex-listings t
      ;;       org-export-latex-listings t)
      (setq org-latex-listings t
            org-export-latex-listings t)
      (setf org-latex-default-packages-alist
            (remove '("AUTO" "inputenc" t)
                    org-latex-default-packages-alist))
      )
    )
  )

(defun my-org/init-ox-bibtex-chinese ()
  (use-package ox-bibtex-chinese
    :if org-enable-bibtex-export
    :config
    (progn
      (when (configuration-layer/layer-used-p 'bibtex)
        (ox-bibtex-chinese-enable))
      )
    )
  )

;; (defun my-org/init-valign ()
;;   (use-package valign
;;     :if org-enable-valign
;;     :hook (org-mode . valign-mode)
;;     )
;;   )


;;; packages.el ends here
