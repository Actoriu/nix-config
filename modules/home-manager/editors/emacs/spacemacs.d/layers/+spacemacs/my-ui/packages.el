;;; packages.el --- UI Layer packages File for Spacemacs
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

(defconst my-ui-packages
  '(
    (awesome-tray :location (recipe
                             :fetcher github
                             :repo "manateelazycat/awesome-tray")
                  :toggle (eq (spacemacs/get-mode-line-theme-name) 'awesome-tray))
    doom-themes
    doom-modeline
    )
  )

(defun my-ui/init-awesome-tray ()
  (spacemacs|unless-dumping-and-eval-after-loaded-dump awesome-tray
    (use-package awesome-tray
      :defer t
      :custom
      (awesome-tray-mode-line-active-color "#A45BAD")
      (awesome-tray-active-modules '("git"
                                     "parent-dir"
                                     "mode-name"
                                     "buffer-name"
                                     "buffer-read-only"
                                     "location"))
      :init
      ;; Prevent flash of unstyled modeline at startup
      (unless after-init-time
        (awesome-tray-mode 1))
      )
    )
  )

(defun my-ui/post-init-doom-themes ()
  (use-package doom-themes
    :custom-face
    (doom-modeline-buffer-file ((t (:inherit (mode-line bold)))))
    :custom
    (doom-themes-treemacs-theme "doom-colors")
    :config
    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; Enable customized theme
    (with-eval-after-load 'lsp-treemacs
      (doom-themes-treemacs-config))
    (doom-themes-org-config)
    )
  )

(defun my-ui/post-init-doom-modeline ()
  (use-package doom-modeline
    :custom
    ;; (doom-modeline-bar-width 3)
    (doom-modeline-hud t)
    (doom-modeline-persp-name nil)
    (doom-modeline-major-mode-icon nil)
    ;; (doom-modeline-buffer-file-name-style 'relative-from-project)
    :init
    ;; Prevent flash of unstyled modeline at startup
    (unless after-init-time
      (setq doom-modeline--default-format mode-line-format)
      (setq-default mode-line-format nil))

    ;; Fix modeline icons in daemon-spawned graphical frames. We have our own
    ;; mechanism for disabling all-the-icons, so we don't need doom-modeline to do
    ;; it for us. However, this may cause unwanted padding in the modeline in
    ;; daemon-spawned terminal frames. If it bothers you, you may prefer
    ;; `doom-modeline-icon' set to `nil'.
    (when (daemonp)
      (setq doom-modeline-icon t))
    )
  )


;;; packages.el ends here
