;;; packages.el --- Chinese Layer packages File for Spacemacs
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

(defconst my-pyim-packages
  '(
    (liberime :location (recipe
                         :fetcher github
                         :repo "merrickluo/liberime"
                         :files ("CMakeLists.txt" "Makefile" "src" "liberime-config.el" "liberime.el"))
              :toggle my-input-method-enable-liberime)
    (pyim :toggle (eq 'pyim my-default-input-method))
    (rime :toggle (eq 'rime my-default-input-method))
    )
  )

(defun my-pyim/init-liberime ()
  (use-package liberime
    :if my-input-method-enable-liberime
    :init
    (progn
      (cond ((and (spacemacs/system-is-linux)
                  (file-directory-p (expand-file-name
                                     "fcitx/rime/"
                                     (getenv "XDG_CONFIG_HOME"))))
             (setq liberime-user-data-dir (expand-file-name
                                           (concat (getenv "XDG_CONFIG_HOME") "/fcitx/rime/"))))))
    :config
    (liberime-select-schema "luna_pinyin_simp")
    )
  )

;; 激活输入法(C-\)
;; 更换输入法(C-x RET C-\ method RET)
;; 显示所有支持的输入法列表(M-x list-input-methods)
(defun my-pyim/init-pyim ()
  (use-package pyim
    :if (eq 'pyim my-default-input-method)
    :diminish pyim-isearch-mode
    ;; :bind (("M-J" . pyim-convert-string-at-point) ;; 与 pyim-probe-dynamic-english 配合
    ;;        ("C-;" . pyim-delete-word-from-personal-buffer))
    :init
    (progn
      (setq pyim-directory (expand-file-name "pyim" spacemacs-cache-directory)
            pyim-dcache-directory (expand-file-name "dcache" pyim-directory)
            pyim-titles '("ㄓ " "PYIM-EN " "PYIM-AU ")
            default-input-method "pyim")
      (evilified-state-evilify pyim-dm-mode pyim-dm-mode-map))
    :config
    (progn
      (setq pyim-fuzzy-pinyin-alist
            '(("en" "eng")
              ("in" "ing")
              ("l" "n")
              ("z" "zh")
              ("c" "ch"))
            pyim-punctuation-translate-p '(no yes auto)
            pyim-page-length 5)

      (if (and (featurep 'liberime)
               my-input-method-enable-liberime)
          (setq pyim-default-scheme 'rime-quanpin)
        (setq pyim-default-scheme 'quanpin))

      ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换速度 :-)
      ;; 我自己使用的中英文动态切换规则是：
      ;; 1. 光标只有在注释里面时，才可以输入中文。
      ;; 2. 光标前是汉字字符时，才能输入中文。
      ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
      (setq-default pyim-english-input-switch-functions
                    '(pyim-probe-dynamic-english
                      pyim-probe-isearch-mode
                      pyim-probe-program-mode
                      pyim-probe-org-structure-template))

      (setq-default pyim-punctuation-half-width-functions
                    '(pyim-probe-punctuation-line-beginning
                      pyim-probe-punctuation-after-punctuation))

      ;; 开启拼音搜索功能
      ;; (pyim-isearch-mode 1)

      ;; 设置选词框的绘制方式
      (if (and (display-graphic-p)
               (>= emacs-major-version 26)
               (featurep 'posframe))
          (setq pyim-page-tooltip 'posframe)
        (setq pyim-page-tooltip 'popup)))))

(defun my-pyim/init-rime()
  (use-package rime
    :if (eq 'rime my-default-input-method)
    :init
    (progn
      (cond ((and (spacemacs/system-is-linux)
                  (file-directory-p (expand-file-name
                                     "fcitx/rime/"
                                     (getenv "XDG_CONFIG_HOME"))))
             (setq rime-user-data-dir (expand-file-name
                                       (concat (getenv "XDG_CONFIG_HOME") "/fcitx/rime/"))))))
    :config
    (progn
      ;; 设置选词框的绘制方式
      (if (and (display-graphic-p)
               (>= emacs-major-version 26)
               (featurep 'posframe))
          (setq rime-show-candidate 'posframe)
        (setq rime-show-candidate 'popup)))
    :custom
    (default-input-method "rime")
    ))


;;; packages.el ends here
