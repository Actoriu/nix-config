;;; packages.el --- W3m Layer packages File for Spacemacs
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

(defconst my-w3m-packages
  '(
    w3m
    (helm-w3m :requires w3m helm)
    ))

(defun my-w3m/init-w3m ()
  (use-package w3m
    :if (executable-find "w3m")
    :defer t
    :commands (w3m)
    :init
    ;; 默认目录
    ;; (setq w3m-profile-directory (concat spacemacs-cache-directory
    ;;                                     (file-name-sans-extension
    ;;                                      (file-name-nondirectory "w3m")))
    ;;       w3m-default-save-directory (concat spacemacs-cache-directory
    ;;                                          (file-name-sans-extension
    ;;                                           (file-name-nondirectory "w3m"))))
    (spacemacs/declare-prefix "an3" "w3m")
    (spacemacs/set-leader-keys
      "an3f" 'w3m-find-file
      "an3s" 'w3m-search)
    (evilified-state-evilify-map w3m-mode-map
      :mode w3m-mode
      :eval-after-load w3m
      :bindings
      "0" 'evil-digit-argument-or-evil-beginning-of-line
      "$" 'evil-end-of-line
      "f" 'evil-find-char
      "F" 'evil-find-char-backward
      "wf" 'w3m-find-file
      "wt" 'w3m-view-this-url-new-session
      "wT" 'w3m-create-empty-session
      "ws" 'w3m-search
      "wS" 'w3m-search-new-session
      "wg" 'w3m-goto-url-new-session
      "wl" 'w3m-next-buffer
      "wh" 'w3m-previous-buffer
      "wx" 'w3m-delete-buffer
      "wD" 'w3m-save-buffer
      "we" 'w3m-bookmark-edit
      "wa" 'w3m-bookmark-add-current-url
      "wm" 'w3m-view-url-with-external-browser
      "wb" 'helm-w3m-bookmarks
      "wB" 'w3m-bookmark-view)
    :config
    (setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533."
          w3m-coding-system 'utf-8
          w3m-file-coding-system 'utf-8
          w3m-file-name-coding-system 'utf-8
          w3m-input-coding-system 'utf-8
          w3m-output-coding-system 'utf-8
          w3m-terminal-coding-system 'utf-8
          w3m-use-favicon nil
          w3m-key-binding 'info
          w3m-mailto-url-function 'compose-mail
          mm-text-html-renderer 'w3m
          ;; 设置w3m主页
          w3m-home-page "https://anyi.life/"
          ;; 设置搜索引擎
          w3m-search-engine-alist '(("baidu" "https://www.baidu.com/s?wd=%s" utf-8)
                                    ("bing" "https://www.bing.com/search?q=%s" utf-8)
                                    ;; https://anyi.life/
                                    ("gugeji" "https://gugeji.com/search?q=%s" utf-8))
          w3m-search-default-engine "gugeji"
          ;; 显示图片
          w3m-default-display-inline-images t
          ;; w3m-default-toggle-inline-images t
          ;; w3m-toggle-inline-images-permanently t
          ;; 显示标题
          w3m-use-header-line-title t
          ;; 显示图标
          w3m-show-graphic-icons-in-header-line t
          ;; w3m-show-graphic-icons-in-mode-line t
          ;; 使用cookies
          w3m-use-cookies t
          ;; 设定w3m运行的参数，分别为使用cookie和使用框架
          w3m-command-arguments '("-F" "-cookie")
          ;; 接收 BAD cookie
          w3m-cookie-accept-bad-cookies t
          ;; 使用w3m作为默认浏览器
          browse-url-browser-function 'w3m-browse-url
          ;; 后台打开连接
          ;; browse-url-browser-function 'w3m-goto-url-new-session
          w3m-view-this-url-new-session-in-background t
          ;; 后台建立新任务
          w3m-new-session-in-background t
          ;; 上次浏览记录的时间显示格式
          w3m-session-time-format "%F %H:%M"
          ;; 使用网站图标的缓存文件
          w3m-favicon-use-cache-file t
          ;; 浏览历史记录的最大值
          w3m-keep-arrived-urls 50000
          ;; 缓存的大小
          w3m-keep-cache-size 1000
          ;; 在其他窗口编辑当前页面
          w3m-edit-function (quote find-file-other-window)
          ;; 退出时自动保存
          w3m-session-automatic-save t
          ;; 关闭一个标签时不保存
          w3m-session-deleted-save nil
          ;; 让标签和创建它的FRAME关联
          w3m-fb-mode t
          ;; 开启过滤
          w3m-use-filter t
          ;; 默认加载崩溃的对话
          w3m-session-load-crashed-sessions t)
    (w3m-fb-mode 1)
    (add-hook 'w3m-fontify-after-hook
              'remove-w3m-output-garbages)
    (setq w3m-image-viewer
          (cond ((spacemacs/system-is-mac) "open")
                ((spacemacs/system-is-linux) "xdg-open")))
    (setq browse-url-generic-program
          (cond ((spacemacs/system-is-mac) "open")
                ((spacemacs/system-is-linux) "xdg-open")))
    )
  )

;; (with-eval-after-load 'w3m
;;   (define-key w3m-mode-map (kbd "C-f") 'evil-scroll-page-down)
;;   (define-key w3m-mode-map (kbd "C-b") 'evil-scroll-page-up)
;;   (define-key w3m-mode-map (kbd "SPC") 'evil-evilified-state))

(defun my-w3m/init-helm-w3m ()
  (use-package helm-w3m
    :commands (helm-w3m-bookmarks)
    :init
    (progn
      (spacemacs/set-leader-keys
        "an3b" 'helm-w3m-bookmarks))))


;;; packages.el ends here
