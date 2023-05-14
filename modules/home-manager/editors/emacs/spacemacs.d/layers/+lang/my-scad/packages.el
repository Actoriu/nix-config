;;; packages.el --- Scad Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst my-scad-packages
  '(
    scad-mode
    (scad-preview :toggle my-scad-mode-enable)
    )
  )

(defun my-scad/post-init-scad-mode ()
  (use-package scad-mode
    :defer t
    :config
    (add-hook 'scad-mode-hook (lambda ()
                                (setq c-basic-offset 4)
                                (highlight-indentation-mode)
                                (setq highlight-indentation-offset 4)))
    (evil-leader/set-key-for-mode 'scad-mode
      "vo" 'scad-open-current-buffer
      "ee" 'scad-preview-export
      "vb" 'scad-open-stl-with-blender)
    )
  )

(defun my-scad/init-scad-preview ()
  (use-package scad-preview
    :if (and (executable-find "openscad")
             my-scad-mode-enable)
    :config
    (setq scad-preview-colorscheme "DeepOcean"
          scad-preview-refresh-delay 1000000  ;; Only refresh manually
          scad-preview-image-size  `( ,(/ (frame-pixel-width) 2)
                                      .
                                      ,(frame-pixel-height))
          scad-preview-window-size (/ (frame-width) 2))
    (evil-leader/set-key-for-mode 'scad-mode
      "vv" 'scad-preview-show
      "vq" 'scad-preview-quit)

    (evil-define-key 'motion scad-preview--image-mode-map
      "k" 'scad-preview-dist-
      "j" 'scad-preview-dist+
      "l" 'scad-preview-rotz-
      "h" 'scad-preview-rotz+
      "q" (lambda () (interactive)
            (bury-buffer)
            (when (window-parent)
              (delete-window)))
      (kbd "C-k") 'scad-preview-rotx+
      (kbd "C-j") 'scad-preview-rotx-)
    )
  )


;;; packages.el ends here
