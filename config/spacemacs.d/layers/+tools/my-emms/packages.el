;;; packages.el --- Emms Layer packages File for Spacemacs
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

(defconst my-emms-packages
  '(
    emms
    (helm-emms :requires helm)
    org-emms
    ;; persp-mode
    )
  )

;; (defun my-emms/pre-init-persp-mode ()
;;   (spacemacs|use-package-add-hook persp-mode
;;     :post-config
;;     (progn
;;       (add-to-list 'persp-filter-save-buffers-functions
;;                    'spacemacs//emms-persp-filter-save-buffers-function)
;;       (spacemacs|define-custom-layout emms-spacemacs-layout-name
;;         :binding emms-spacemacs-layout-binding
;;         :body
;;         (progn
;;           (dolist (mode emms-modes)
;;             (let ((hook (intern (concat (symbol-name mode) "-hook"))))
;;               (add-hook hook #'spacemacs//emms-buffer-to-persp)))
;;           (call-interactively 'emms))))))

(defun my-emms/init-emms ()
  (use-package emms
    :defer t
    :init
    (progn
      (setq emms-directory (expand-file-name "emms" spacemacs-cache-directory))
      (spacemacs/declare-prefix "am" "Media")
      (spacemacs/declare-prefix "ame" "EMMS")
      (spacemacs/set-leader-keys
        "ames" 'emms-streams
        "ameb" 'emms-browser
        "amep" 'emms-playlist-mode-go
        "ameo" 'emms-show
        "a SPC" 'emms-play-pause-dwim
        "a ." 'emms-next
        "a ," 'emms-previous
        "a RET" 'emms-smart-browse)
      )
    :config
    (progn
      (emms-all)
      (emms-default-players)
      (emms-mode-line-disable)
      (setq emms-source-file-default-directory
            (file-name-as-directory
             (expand-file-name "Videos" user-home-directory))
            ;; Cover thumbnails.
            emms-browser-covers 'emms-browser-cache-thumbnail-async)
      (when (executable-find "find")
        (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find))
      ;; Track information, function `emms-info-libtag' need emms-print-metadata
      ;; command in emms, which build with: make emms-print-metadata
      (if (executable-find "emms-print-metadata")
          (progn
            (require 'emms-info-libtag)
            (add-to-list 'emms-info-functions 'emms-info-libtag)
            (delete 'emms-info-ogginfo emms-info-functions)
            (delete 'emms-info-mp3info emms-info-functions))
        (add-to-list 'emms-info-functions 'emms-info-ogginfo)
        (add-to-list 'emms-info-functions 'emms-info-mp3info)
        )
      ;; (cond ((and emms-enable-player-mpv
      ;;             (executable-find "mpv"))
      ;;        (setq emms-player-list '(emms-player-mpv)
      ;;              emms-player-mpv-update-metadata t)
      ;;        ;; (when (executable-find "pulseaudio")
      ;;        ;;   (setq emms-player-mpv-environment
      ;;        ;;         '("PULSE_PROP_media.role=music")))
      ;;        )
      ;;       )
      (add-to-list 'emms-info-functions 'emms-info-cueinfo)

      (evilified-state-evilify-map emms-browser-mode-map
        :mode emms-browser-mode
        :bindings
        ;; since this is normally SPC
        "t" 'emms-browser-toggle-subitems
        ;; makes more sense than C-j
        (kbd "<S-return>") 'emms-browser-add-tracks-and-play
        )

      (evilified-state-evilify-map emms-mark-mode-map
        :mode emms-mark-mode
        :bindings
        "t" 'emms-mark-toggle
        "u" 'emms-mark-unmark-forward
        "K" 'emms-mark-kill-marked-tracks
        "M" 'emms-mark-mode-disable
        )

      (evilified-state-evilify-map emms-playlist-mode-map
        :mode emms-playlist-mode
        :bindings
        "l" 'emms-next
        "h" 'emms-previous
        "H" 'emms-playlist-mode-first
        "L" 'emms-playlist-mode-last
        "W" 'emms-playlist-save
        ;; P also works for emms-pause but it's kind of a stupid binding.
        ;; can't use SPC, so we'll make do with TAB
        (kbd "TAB") 'emms-pause
        "," 'emms-seek-minute-backward
        "." 'emms-seek-minute-forward
        "u" 'emms-playlist-mode-undo
        "p" 'emms-playlist-mode-yank
        "P" 'emms-playlist-mode-yank-pop
        "O" 'emms-playlist-mode-insert-newline
        ;; having trouble with this because it is
        ;; sometimes calling 'emms-playlist-mode-current-kill
        "K" 'emms-mark-kill-marked-tracks
        "M" 'emms-mark-mode
        )

      )
    )
  )

(defun my-emms/init-helm-emms ()
  (use-package helm-emms
    :after (emms helm)
    :config
    (setq helm-emms-default-sources
          '(helm-source-emms-dired
            ;; Disable for a huge speed-up.
            helm-source-emms-files
            helm-source-emms-streams))
    )
  )

(defun my-emms/init-org-emms ()
  (use-package org-emms
    :after emms))


;;; packages.el ends here
