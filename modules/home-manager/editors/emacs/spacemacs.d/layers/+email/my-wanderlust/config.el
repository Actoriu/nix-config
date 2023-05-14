;;; config.el --- Wanderlust configuration File for Spacemacs
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

(defvar wanderlust-spacemacs-layout-name "@Wanderlust"
  "Name used in the setup for `spacemacs-layouts' micro-state")

(defvar wanderlust-spacemacs-layout-binding "w"
  "Binding used in the setup for `spacemacs-layouts' micro-state")

(defvar wanderlust-modes
  '(wl-draft-mode
    wl-draft-editor-mode
    wl-folder-mode
    wl-highlight-background-mode
    wl-message-mode
    wl-message-decode-mode
    wl-original-message-mode
    wl-plugged-mode
    wl-score-mode
    wl-summary-mode
    wl-summary-buffer-display-header-mode
    wl-summary-buffer-display-mime-mode
    wl-template-mode
    mime-edit-mode
    mime-view-mode)
  "Modes that are associated with wanderlust buffers.")

(defvar my-wanderlust-passwd-enable-auth-source nil
  "If non-nil use auto-source for passwd-storage are enabled.")

(defvar my-wanderlust-enable-org nil
  "If non-nil use org-mode for wanderlust send email are enabled.")

(defvar my-wanderlust-enable-stay-folder-window nil
  "If non-nil wl-stay-folder-window are enabled.")

(defvar my-wanderlust-enable-w3m-preview nil
  "If non-nil mime preview setup w3m are enabled.")

(defvar my-wanderlust-enable-shr-preview nil
  "If non-nil mime preview setup eww are enabled.")

(defvar my-wanderlust-enable-search-cjk nil
  "if non-nil Index and search emails written in CJK scripts are enabled.")

(defvar my-wanderlust-enable-x-face nil
  "If non-nil mime preview setup x-face are enabled.")


;;; config.el ends here
