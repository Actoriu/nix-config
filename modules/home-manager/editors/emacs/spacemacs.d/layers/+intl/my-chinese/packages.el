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

(defconst my-chinese-packages
  '(
    (cnfonts :toggle my-chinese-enable-cnfonts)
    ;; (chinese-calendar :location local)
    (calendar :location built-in)
    cal-china-x
    (liberime :location (recipe
                         :fetcher github
                         :repo "merrickluo/liberime"
                         :files ("CMakeLists.txt" "Makefile" "src" "liberime-config.el" "liberime.el"))
              :toggle my-input-method-enable-liberime)
    ;; (pyim :toggle (eq 'pyim my-default-input-method))
    pyim
    (rime :toggle (eq 'rime my-default-input-method))
    (smart-input-source :toggle my-input-method-enable-smart-input-source)
    (evil-pinyin :toggle my-chinese-enable-evil-pinyin)
    )
  )

(defun my-chinese/init-cnfonts ()
  (use-package cnfonts
    :demand t
    :if (and (getenv "DISPLAY") my-chinese-enable-cnfonts)
    ;; :after all-the-icons
    :hook (cnfonts-set-font-finish
           . (lambda (fontsizes-list) ;; HTTP://github.com/seagle0128/doom-modeline/issues/278
               (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
               (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
               (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
               (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
               (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
               (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append)))
    :init
    (progn
      (let* ((my-cnfonts-dir
              (configuration-layer/get-layer-local-dir 'my-chinese)))
        (setq cnfonts-directory (file-name-as-directory
                                 (expand-file-name
                                  "cnfonts"
                                  my-cnfonts-dir))))
      (setq cnfonts-verbose nil))
    :config
    (progn
      (when (featurep 'org)
        (setq cnfonts-use-face-font-rescale (spacemacs/system-is-linux)))
      (cnfonts-set-spacemacs-fallback-fonts)
      (cnfonts-enable)
      )
    )
  )

;; (defun my-chinese/init-chinese-calendar ()
;;   (use-package chinese-calendar)
;;   )

(defun my-chinese/init-calendar ()
  (use-package calendar
    :config
    (progn
      (setq calendar-location-name "Ningzhou, China"
            calendar-time-zone +480
            calendar-latitude 24.196079
            calendar-longtitude 102.927048
            mark-holidays-in-calendar t
            calendar-mark-holidays-flag t
            calendar-chinese-all-holidays-flag t)
      )
    )
  )

(defun my-chinese/init-cal-china-x ()
  (use-package cal-china-x
    :after calendar
    :commands cal-china-x-setup
    :init (cal-china-x-setup)
    :config
    (progn
      (setq cal-china-x-important-holidays
            '(;; 公历节日
              (holiday-fixed 1 22 "母亲生日")
              (holiday-fixed 2 17 "爷爷忌日")
              (holiday-fixed 5 11 "父亲生日")
              (holiday-fixed 6 13 "弟弟生日")
              (holiday-fixed 11 19 "我的生日")
              ;; 农历节日
              (holiday-lunar 1 1 "春节" 0)
              (holiday-lunar 1 15 "元宵(上元节)" 0)
              (holiday-lunar 2 2 "龙抬头" 0)
              (holiday-lunar 3 3 "上巳节" 0)
              (holiday-lunar 5 5 "端午节" 0)
              (holiday-lunar 7 7 "七夕节" 0)
              (holiday-lunar 7 15 "中元节" 0)
              (holiday-lunar 8 15 "中秋节" 0)
              (holiday-lunar 9 9 "重阳节" 0)
              (holiday-lunar 10 1 "寒衣节" 0)
              (holiday-lunar 10 15 "下元节" 0)
              (holiday-lunar 12 8 "腊八节" 0)
              (holiday-lunar 12 23 "小年" 0)
              (holiday-lunar 12 30 "除夕" 0)
              (holiday-solar-term "立春" "立春")
              (holiday-solar-term "雨水" "雨水")
              (holiday-solar-term "惊蛰" "惊蛰")
              (holiday-solar-term "春分" "春分")
              (holiday-solar-term "清明" "清明节")
              (holiday-solar-term "谷雨" "谷雨")
              (holiday-solar-term "立夏" "立夏")
              (holiday-solar-term "小满" "小满")
              (holiday-solar-term "芒种" "芒种")
              (holiday-solar-term "夏至" "夏至")
              (holiday-solar-term "小暑" "小暑")
              (holiday-solar-term "大暑" "大暑")
              (holiday-solar-term "立秋" "立秋")
              (holiday-solar-term "处暑" "处暑")
              (holiday-solar-term "白露" "白露")
              (holiday-solar-term "秋分" "秋分")
              (holiday-solar-term "寒露" "寒露")
              (holiday-solar-term "霜降" "霜降")
              (holiday-solar-term "立冬" "立冬")
              (holiday-solar-term "小雪" "小雪")
              (holiday-solar-term "大雪" "大雪")
              (holiday-solar-term "冬至" "冬至")
              (holiday-solar-term "小寒" "小寒")
              (holiday-solar-term "大寒" "大寒")
              )
            cal-china-x-general-holidays
            '(;; 公历节日
              (holiday-fixed 1 1 "元旦")
              (holiday-fixed 2 14 "情人节")
              (holiday-fixed 3 8 "妇女节")
              (holiday-fixed 3 12 "植树节")
              (holiday-fixed 3 14 "白色情人节")
              (holiday-fixed 4 1 "愚人节")
              (holiday-fixed 5 1 "劳动节")
              (holiday-fixed 5 4 "青年节")
              (holiday-float 5 0 2 "母亲节")
              (holiday-fixed 6 1 "儿童节")
              (holiday-float 6 0 3 "父亲节")
              (holiday-fixed 7 1 "建党节")
              (holiday-fixed 8 1 "建军节")
              (holiday-fixed 9 10 "教师节")
              (holiday-fixed 10 1 "国庆节")
              (holiday-float 11 4 4 "感恩节")
              (holiday-fixed 12 24 "平安夜")
              (holiday-fixed 12 25 "圣诞节")
              )
            calendar-holidays (append cal-china-x-important-holidays
                                      cal-china-x-general-holidays)
            )
      )
    )
  )

(defun my-chinese/init-liberime ()
  (use-package liberime
    :if (and (not (equal system-configuration "aarch64-unknown-linux-android"))
             my-input-method-enable-liberime)
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
(defun my-chinese/post-init-pyim ()
  (use-package pyim
    ;; :if (eq 'pyim my-default-input-method)
    :diminish pyim-isearch-mode
    ;; :bind (("M-J" . pyim-convert-string-at-point) ;; 与 pyim-probe-dynamic-english 配合
    ;;        ("C-;" . pyim-delete-word-from-personal-buffer))
    :init
    (progn
      (setq pyim-titles '("ㄓ " "PYIM-EN " "PYIM-AU "))
      )
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

(defun my-chinese/init-rime()
  (use-package rime
    :if (and (not (equal system-configuration "aarch64-unknown-linux-android"))
             (eq 'rime my-default-input-method))
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

(defun my-chinese/init-smart-input-source ()
  (use-package smart-input-source
    :if (and (not (equal system-configuration "aarch64-unknown-linux-android"))
             (eq 'fcitx5 my-input-method-enable-smart-input-source))
    :config
    (progn
      (smart-input-source-ism-lazyman-config nil nil 'fcitx5)
      ;; enable the /cursor color/ mode
      (smart-input-source-global-cursor-color-mode t)
      ;; enable the /respect/ mode
      (smart-input-source-global-respect-mode t)
      ;; enable the /follow context/ mode for all buffers
      (smart-input-source-global-follow-context-mode t)
      ;; enable the /inline english/ mode for all buffers
      (smart-input-source-global-inline-mode t)
      )
    )
  )

(defun my-chinese/init-evil-pinyin ()
  (use-package evil-pinyin
    :after (evil)
    :config
    (global-evil-pinyin-mode)
    )
  )


;;; packages.el ends here
