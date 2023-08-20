;;; config.el --- Emms Layer configuration File for Spacemacs
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

(defvar emms-spacemacs-layout-name "@Emms"
  "Name used in the setup for `spacemacs-layouts' micro-state")

(defvar emms-spacemacs-layout-binding "e"
  "Binding used in the setup for `spacemacs-layouts' micro-state")

(defvar emms-modes
  '(emms-activate-highlighting-mode
    emms-browser-mode
    emms-lyrics-mode
    emms-mark-mode
    emms-metaplaylist-mode
    emms-playlist-mode
    emms-show-all-mode
    emms-stream-mode
    emms-tag-editor-mode
    emms-volume-minor-mode)
  "Modes that are associated with emms buffers.")

(defvar emms-enable-player-mpv nil
  "If non nil emms-player-mpv while startup.")


;;; config.el ends here
