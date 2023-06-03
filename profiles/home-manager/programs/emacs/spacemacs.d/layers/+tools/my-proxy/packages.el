;;; packages.el --- Proxy Layer packages File for Spacemacs
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

(defconst my-proxy-packages
  '(
    (proxy-mode :toggle my-proxy-enable)
    (with-proxy :toggle my-proxy-enable)
    )
  )

(defun my-proxy/init-proxy-mode ()
  (use-package proxy-mode
    :if my-proxy-enable
    :defer t
    :commands (proxy-mode)
    :init
    (setq proxy-mode-socks-proxy '("Default server" "127.0.0.1" 1089 5))
    )
  )

(defun my-proxy/init-with-proxy ()
  (use-package with-proxy
    :if my-proxy-enable
    :defer t
    :commands (with-proxy with-proxy-url with-proxy-shell)
    ;; Privoxy
    :config
    (setq with-proxy-http-server "127.0.0.1:8889")
    )
  )


;;; packages.el ends here
