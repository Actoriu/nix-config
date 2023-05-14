;;; packages.el --- Eww Layer packages File for Spacemacs
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

(defconst my-eww-packages
  '(
    (eww :location built-in)
    (helm-eww :requires eww helm)
    ))

(defun my-eww/init-eww ()
  (use-package eww
    :defer t
    :init
    (spacemacs/set-leader-keys
      "ane" 'eww
      "anb" 'eww-list-bookmarks)
    (evilified-state-evilify-map eww-mode-map
      :mode eww-mode
      :eval-after-load eww
      :bindings
      "l" 'evil-forward-char
      "i" 'evil-insert
      "H" 'eww-back-url
      "L" 'eww-forward-url
      "f" 'eww-lnum-follow ;; ace-link-eww
      "F" 'eww-lnum-universal
      "o" 'eww
      "Y" 'eww-copy-page-url
      ;; "p" TODO: open url on clipboard
      "r" 'eww-reload
      "b" 'helm-eww-bookmarks
      "a" 'eww-add-bookmark)
    :config
    (progn
      (setq eww-bookmarks-directory
            (file-name-as-directory
             (expand-file-name "url" spacemacs-cache-directory))
            ;; url-http-user-agent-string "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533."
            eww-search-prefix "https://www.baidu.com/s?wd=%s"
            shr-external-browser 'eww-browse-url)
      ;; 背景色
      (advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
      (advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
      ;; 禁用图片浏览
      (add-hook 'eww-mode-hook 'eww-mode-hook--disable-image)
      )
    )
  )

(defun my-eww/init-helm-eww ()
  (use-package helm-eww
    :defer t
    )
  )


;;; packages.el ends here
