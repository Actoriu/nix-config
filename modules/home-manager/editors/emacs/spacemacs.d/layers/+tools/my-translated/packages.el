;;; packages.el --- Translated Layer packages File for Spacemacs
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

(defconst my-translated-packages
  '(
    (company-english-helper :location (recipe
                                       :fetcher github
                                       :repo "manateelazycat/company-english-helper"))
    (english-teacher :location (recipe
                                :fetcher github
                                :repo "loyalpartner/english-teacher.el"))
    (insert-translated-name :location (recipe
                                       :fetcher github
                                       :repo "manateelazycat/insert-translated-name"))
    (popweb :location (recipe
                       :fetcher github
                       :repo "manateelazycat/popweb"
                       :files ("extension" "*.el" "*.md" "*.py" "*.js"))
            :toggle (and (configuration-layer/layer-used-p 'my-eaf)
                         (my-eaf-detect)))
    (sdcv :location (recipe
                     :fetcher github
                     :repo "manateelazycat/sdcv")
          :toggle (my-translated-sdcv-toggle))
    )
  )

(defun my-translated/init-company-english-helper ()
  (use-package company-english-helper))

(defun my-translated/init-english-teacher ()
  (use-package english-teacher
    :hook
    ((Info-mode
      elfeed-show-mode
      Man-mode
      Woman-Mode) . english-teacher-follow-mode)
    :config
    (setq english-teacher-show-result-function 'english-teacher-eldoc-show-result-function
          english-teacher-backend 'baidu
          english-teacher-disabled-functions
          '((lambda ()
              (and
               (or (string-suffix-p ".h" (buffer-file-name))
                   (string-suffix-p ".cc" (buffer-file-name)))
               (not (english-teacher-in-comment-p))))))
    ))

(defun my-translated/init-insert-translated-name ()
  (use-package insert-translated-name))

(defun my-translated/init-popweb ()
  (use-package popweb
    :if (and (configuration-layer/layer-used-p 'my-eaf)
             (my-eaf-toggle))
    :config
    (popweb-add-subdirs-to-load-path))
  (use-package popweb-dict-bing
    :after popweb
    ;; :load-path (lambda ()
    ;;              (expand-file-name
    ;;               "extension/dict"
    ;;               (configuration-layer//get-package-directory 'popweb)))
    )
  (use-package popweb-dict-youdao
    :after popweb
    ;; :load-path (lambda ()
    ;;              (expand-file-name
    ;;               "extension/dict"
    ;;               (configuration-layer//get-package-directory 'popweb)))
    )
  (use-package popweb-latex
    :after popweb
    ;; :load-path (lambda ()
    ;;              (expand-file-name
    ;;               "extension/latex"
    ;;               (configuration-layer//get-package-directory 'popweb)))
    :hook (latex-mode . popweb-latex-mode))
  )

(defun my-translated/init-sdcv ()
  (use-package sdcv
    :if (and (featurep 'posframe) (my-translated-sdcv-toggle))
    :commands (sdcv-search-pointer sdcv-search-pointer+ sdcv-search-input sdcv-search-input+)
    :custom-face (sdcv-tooltip-face ((t (:foreground "#B2B2B2" :background "#5E5079"))))
    :hook (sdcv-mode . visual-line-mode)
    :config
    (progn
      (let* ((my-translated-dictionary-dir
              (configuration-layer/get-layer-local-dir 'my-translated))
             (my-translated-layer-sdcv-dictionary-dir (file-name-as-directory
                                                       (expand-file-name
                                                        "sdcv-dict"
                                                        my-translated-dictionary-dir))))
        (setq sdcv-dictionary-data-dir my-translated-layer-sdcv-dictionary-dir))
      (setq sdcv-say-word-p t
            ;; 星际译王屏幕取词词典, 简单, 快速
            sdcv-dictionary-simple-list
            '("简明英汉字典增强版"
              "懒虫简明英汉词典"
              "懒虫简明汉英词典"
              "朗道汉英字典5.0"
              "朗道英汉字典5.0")
            ;; 星际译王的词典, 完全, 详细
            sdcv-dictionary-complete-list
            '("简明英汉字典增强版"
              "懒虫简明英汉词典"
              "英汉汉英专业词典"
              "XDICT英汉辞典"
              "stardict1.3英汉辞典"
              "WordNet"
              "XDICT汉英辞典"
              "Jargon"
              "懒虫简明汉英词典"
              "Free"
              "新世纪英汉科技大词典"
              "KDic11万英汉词典"
              "朗道汉英字典5.0"
              "朗道英汉字典5.0"
              "CDICT5英汉辞典"
              "新世纪汉英科技大词典"
              "牛津英汉双解美化版"
              "21世纪双语科技词典"
              "quick_eng-zh_CN"))
      )
    )
  )


;;; packages.el ends here
