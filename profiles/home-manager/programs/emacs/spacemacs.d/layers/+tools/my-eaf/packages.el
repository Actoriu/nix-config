;;; packages.el --- Emacs Application Framework Layer packages File for Spacemacs
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

(defconst my-eaf-packages
  '(
    (color-rg :location (recipe
                         :fetcher github
                         :repo "manateelazycat/color-rg"))
    (eaf :location (recipe
                    :fetcher github
                    :repo "emacs-eaf/emacs-application-framework"
                    :files ("core" "extension" "*.el" "*.py" "*.json" "*.md" "LICENSE"))
         :toggle (my-eaf-detect))
    ;; (eaf :location local
    ;;      :toggle (my-eaf-detect))
    ;; yasnippet
    ;; (lsp-bridge :requires markdown-mode posframe yasnippet
    ;;             :location (recipe
    ;;                        :fetcher github
    ;;                        :repo "manateelazycat/lsp-bridge"
    ;;                        :files ("acm" "core" "langserver" "multiserver" "resources" "*.el" "*.py" "*.md"))
    ;;             :toggle (and
    ;;                      (not (configuration-layer/layer-used-p 'auto-completion))
    ;;                      (not (configuration-layer/layer-used-p 'lsp))
    ;;                      (my-eaf-python-detect)
    ;;                      (my-eaf-pip-detect)))
    ;; popon
    ;; It shouldn't be installed with quelpa, use git clone /path/to/acm-terminal instead.
    ;; https://github.com/twlz0ne/acm-terminal/issues/1
    ;; (acm-terminal :requires lsp-bridge popon
    ;;               :location (recipe
    ;;                          :fetcher github
    ;;                          :repo "twlz0ne/acm-terminal"
    ;;                          :files ("*.el"))
    ;;               :toggle (configuration-layer/package-used-p 'lsp-bridge))
    (fuz :toggle (my-eaf-fuz-toggle))
    (helm-fuz :requires helm
              :toggle (and (configuration-layer/layer-used-p 'helm)
                           (my-eaf-fuz-toggle)))
    netease-cloud-music
    (snails :location (recipe
                       :fetcher github
                       :repo "manateelazycat/snails"
                       :files (:defaults "*.sh" "*.ps1"))
            :toggle (my-eaf-snails-toggle))
    ))

(defun my-eaf/init-color-rg ()
  (use-package color-rg))

(defun my-eaf/init-eaf ()
  (use-package eaf
    :if (my-eaf-toggle)
    :custom
    (browse-url-browser-function 'eaf-open-browser)
    (eaf-browser-continue-where-left-off t)
    (eaf-browser-enable-adblocker t)
    (eaf-browser-enable-autofill t)
    (eaf-browser-remember-history nil)
    ;; (eaf-webengine-default-zoom (if (> (frame-pixel-width) 3000) 2.3 1))
    (setq eaf-webengine-font-family "Sarasa Gothic SC")
    (setq eaf-webengine-fixed-font-family "Sarasa Fixed SC")
    (setq eaf-webengine-serif-font-family "Sarasa Fixed Slab SC")
    (setq eaf-webengine-font-size 18)
    (setq eaf-webengine-fixed-font-size 18)
    (eaf-jupyter-font-family "Sarasa Fixed SC")
    (eaf-jupyter-font-size 18)
    (eaf-marker-fontsize 18)
    (eaf-music-default-file "~/Music")
    (eaf-music-cache-dir "~/Music")
    (eaf-pdf-marker-fontsize 18)
    (eaf-terminal-font-family "Sarasa Fixed SC")
    (eaf-terminal-font-size 18)
    (eaf-pyqterminal-font-family "Sarasa Fixed SC")
    (eaf-pyqterminal-font-size 24)
    :init
    (progn
      (setq eaf-config-location (expand-file-name "eaf" spacemacs-cache-directory))

      (spacemacs/declare-prefix
        "aa"  "application-framework"
        "aab" "browser"
        "aabq" "quick-launch-website"
        "aam" "mindmap")

      (spacemacs/set-leader-keys "aac" 'eaf-open-camera)
      (spacemacs/set-leader-keys "aaf" 'eaf-open)
      (spacemacs/set-leader-keys "aaj" 'eaf-open-jupyter)
      (spacemacs/set-leader-keys "aao" 'eaf-open-office)
      (spacemacs/set-leader-keys "aat" 'eaf-open-terminal)
      (spacemacs/set-leader-keys "aas" 'eaf-open-system-monitor)
      (spacemacs/set-leader-keys "aaM" 'eaf-open-music-player)

      (spacemacs/set-leader-keys "aabo" 'eaf-open-browser)
      (spacemacs/set-leader-keys "aabs" 'eaf-search-it)
      (spacemacs/set-leader-keys "aabb" 'eaf-open-bookmark)
      (spacemacs/set-leader-keys "aabh" 'eaf-open-browser-with-history)

      (spacemacs/set-leader-keys "aabqd" 'duckduckgo)
      (spacemacs/set-leader-keys "aabqw" 'wikipedia)
      (spacemacs/set-leader-keys "aabqy" 'youtube)
      (spacemacs/set-leader-keys "aamc" 'eaf-create-mindmap)
      (spacemacs/set-leader-keys "aamm" 'eaf-open-mindmap)

      (setq eaf-browser-keybinding
            '(("C--" . "zoom_out")
              ("C-=" . "zoom_in")
              ("C-0" . "zoom_reset")
              ("C-s" . "search_text_forward")
              ("C-r" . "search_text_backward")
              ("C-n" . "scroll_up")
              ("C-p" . "scroll_down")
              ("C-f" . "scroll_right")
              ("C-b" . "scroll_left")
              ("C-v" . "scroll_up_page")
              ("C-y" . "yank_text")
              ("C-w" . "kill_text")
              ("M-e" . "atomic_edit")
              ("M-c" . "caret_toggle_browsing")
              ("M-D" . "select_text")
              ("M-s" . "open_link")
              ("M-S" . "open_link_new_buffer")
              ("M-B" . "open_link_background_buffer")
              ("C-/" . "undo_action")
              ("M-_" . "redo_action")
              ("M-w" . "copy_text")
              ("M-f" . "history_forward")
              ("M-b" . "history_backward")
              ("M-q" . "clear_cookies")
              ("C-t" . "toggle_password_autofill")
              ("C-d" . "save_page_password")
              ("M-a" . "toggle_adblocker")
              ("C-M-q" . "clear_history")
              ("C-M-i" . "import_chrome_history")
              ("M-v" . "scroll_down_page")
              ("M-<" . "scroll_to_begin")
              ("M->" . "scroll_to_bottom")
              ("M-p" . "duplicate_page")
              ("M-t" . "new_blank_page")
              ("M-d" . "toggle_dark_mode")
              ("<" . "insert_or_select_left_tab")
              (">" . "insert_or_select_right_tab")
              ("j" . "insert_or_scroll_up")
              ("k" . "insert_or_scroll_down")
              ("h" . "insert_or_scroll_left")
              ("l" . "insert_or_scroll_right")
              ("f" . "insert_or_open_link")
              ("F" . "insert_or_open_link_new_buffer")
              ("B" . "insert_or_open_link_background_buffer")
              ("c" . "insert_or_caret_at_line")
              ("J" . "insert_or_scroll_up_page")
              ("K" . "insert_or_scroll_down_page")
              ("H" . "insert_or_history_backward")
              ("L" . "insert_or_history_forward")
              ("t" . "insert_or_new_blank_page")
              ("T" . "insert_or_recover_prev_close_page")
              ("i" . "insert_or_focus_input")
              ("I" . "insert_or_open_downloads_setting")
              ("r" . "insert_or_refresh_page")
              ("g" . "insert_or_scroll_to_begin")
              ("x" . "insert_or_close_buffer")
              ("G" . "insert_or_scroll_to_bottom")
              ("-" . "insert_or_zoom_out")
              ("=" . "insert_or_zoom_in")
              ("0" . "insert_or_zoom_reset")
              ;; ("d" . "insert_or_dark_mode")
              ("m" . "insert_or_save_as_bookmark")
              ("o" . "insert_or_open_browser")
              ;; ("y" . "insert_or_download_youtube_video")
              ("y" . "insert_or_copy_text")
              ("Y" . "insert_or_download_youtube_audio")
              ("p" . "insert_or_toggle_device")
              ("P" . "insert_or_duplicate_page")
              ("1" . "insert_or_save_as_pdf")
              ("2" . "insert_or_save_as_single_file")
              ("v" . "insert_or_view_source")
              ("e" . "insert_or_edit_url")
              ("C-M-c" . "copy_code")
              ("C-M-l" . "copy_link")
              ("C-a" . "select_all_or_input_text")
              ("M-u" . "clear_focus")
              ("C-j" . "open_downloads_setting")
              ("M-o" . "eval_js")
              ("M-O" . "eval_js_file")
              ("<escape>" . "eaf-browser-send-esc-or-exit-fullscreen")
              ("M-," . "eaf-send-down-key")
              ("M-." . "eaf-send-up-key")
              ("M-m" . "eaf-send-return-key")
              ("<f5>" . "refresh_page")
              ("<f12>" . "open_devtools")
              ("<C-return>" . "eaf-send-ctrl-return-sequence")))

      (setq eaf-pdf-viewer-keybinding
            '(("j" . "scroll_up")
              ("<down>" . "scroll_up")
              ("C-n" . "scroll_up")
              ("k" . "scroll_down")
              ("<up>" . "scroll_down")
              ("C-p" . "scroll_down")
              ("h" . "scroll_left")
              ("<left>" . "scroll_left")
              ("C-b" . "scroll_left")
              ("l" . "scroll_right")
              ("<right>" . "scroll_right")
              ("C-f" . "scroll_right")
              ("J" . "scroll_up_page")
              ("K" . "scroll_down_page")
              ("C-v" . "scroll_up_page")
              ("M-v" . "scroll_down_page")
              ("t" . "toggle_read_mode")
              ("0" . "zoom_reset")
              ("=" . "zoom_in")
              ("-" . "zoom_out")
              ("g" . "scroll_to_begin")
              ("G" . "scroll_to_end")
              ("p" . "jump_to_page")
              ("P" . "jump_to_percent")
              ("[" . "save_current_pos")
              ("]" . "jump_to_saved_pos")
              ("i" . "toggle_inverted_mode")
              ("m" . "toggle_mark_link")
              ("f" . "jump_to_link")
              ("d" . "toggle_inverted_mode")
              ("M-w" . "copy_select")
              ("C-s" . "search_text_forward")
              ("C-r" . "search_text_backward")
              ("x" . "close_buffer")
              ("C-<right>" . "rotate_clockwise")
              ("C-<left>" . "rotate_counterclockwise")
              ("M-h" . "add_annot_highlight")
              ("M-u" . "add_annot_underline")
              ("M-s" . "add_annot_squiggly")
              ("M-d" . "add_annot_strikeout_or_delete_annot")
              ("M-e" . "add_annot_text_or_edit_annot")
              ("M-p" . "toggle_presentation_mode")
              ("o" . "eaf-pdf-outline"))))
    ;; switch tab only works with awesome-tab package
    ;; ("<C-tab>" . "select_left_tab")
    ;; ("<C-iso-lefttab>" . "select_right_tab")
    :config
    (progn
      (dolist (app eaf-apps)
        (require app nil 'noerror))

      (define-key eaf-mode-map* (kbd "C-SPC C-SPC") 'execute-extended-command)
      ;;;; TODO need to consider the current pdf view mode which does not need to be pdf view mode
      (spacemacs/set-leader-keys-for-major-mode 'pdf-view-mode "E" 'spacemacs/open-with-eaf)
      (add-to-list 'evil-evilified-state-modes 'eaf-pdf-outline-mode)))

  ;; remove compiled file necessary to suppress clear_focus variable non-existent
  ;; error (eaf is not yet meant to be installed with quelpa, see
  ;; `https://github.com/manateelazycat/emacs-application-framework#install')

  ;; Alternative way to delete compiled file
  ;; (when (locate-library "eaf-evil.elc")
  ;;   (delete-file))

  (use-package eaf-evil
    :after eaf
    :config
    (progn
      ;; the following line are taken from the evil-integration example:
      ;; https://github.com/manateelazycat/emacs-application-framework/wiki/Evil
      (setq eaf-evil-leader-keymap spacemacs-cmds)

      (define-key key-translation-map (kbd "SPC")
        (lambda (prompt)
          (if (derived-mode-p 'eaf-mode)
              (pcase eaf--buffer-app-name
                ((or
                  (and "browser"
                       (guard (not (eaf-call-sync "execute_function" eaf--buffer-id "is_focus"))))
                  "image-viewer"
                  "pdf-viewer")
                 (kbd eaf-evil-leader-key))
                (_  (kbd "SPC")))
            (kbd "SPC"))))

      ;; The following lines create the major-mode leader key emulation map
      ;; in a similar way as how it was done in the evil-integration example
      (setq eaf-evil-leader-for-major-keymap (make-sparse-keymap))
      (define-key eaf-evil-leader-for-major-keymap (kbd "h") 'eaf-open-browser-with-history)
      (define-key eaf-evil-leader-for-major-keymap (kbd "d") 'eaf-proxy-toggle_dark_mode)
      (define-key eaf-evil-leader-for-major-keymap (kbd "s") 'eaf-search-it)
      (add-hook 'evil-normal-state-entry-hook
                (lambda ()
                  (when (derived-mode-p 'eaf-mode)
                    (define-key eaf-mode-map (kbd "C-,") eaf-evil-leader-for-major-keymap))))

      (define-key key-translation-map (kbd ",")
        (lambda (prompt)
          (if (derived-mode-p 'eaf-mode)
              (if (eaf-call-sync "execute_function" eaf--buffer-id "is_focus")
                  (kbd ",")
                (kbd "C-,")))))
      )
    )

  (use-package eaf-org
    :after eaf
    :config
    (progn
      ;; use `emacs-application-framework' to open PDF file: link
      (add-to-list 'org-file-apps '("\\.pdf\\'" . eaf-org-open-file))
      )
    )
  )

;; (defun my-eaf/init-yasnippet ()
;;   (use-package yasnippet
;;     :commands (yas-global-mode yas-minor-mode yas-activate-extra-mode)
;;     :defer t
;;     :config
;;     (yas-global-mode 1)
;;     ))

;; (defun my-eaf/init-popon ()
;;   (use-package popon))

;; (defun my-eaf/init-lsp-bridge ()
;;   (use-package lsp-bridge
;;     :commands (lsp-bridge-mode)
;;     :custom
;;     ;; (lsp-bridge-enable-auto-import t)
;;     (acm-candidate-match-function 'orderless-flex)
;;     (lsp-bridge-enable-mode-line nil)
;;     :init
;;     (global-lsp-bridge-mode)
;;     :config
;;     (progn
;;       ;; (setq lsp-bridge-python-command "/bin/python")

;;       ;; It shouldn't be installed with quelpa, use git clone /path/to/acm-terminal instead.
;;       (unless (display-graphic-p)
;;         (add-to-list 'load-path (expand-file-name
;;                                  "acm-terminal"
;;                                  (configuration-layer/get-layer-local-dir 'my-eaf)))
;;         (with-eval-after-load 'acm
;;           (require 'acm-terminal)))

;;       ;; 融合 `lsp-bridge' `find-function' 以及 `dumb-jump' 的智能跳转
;;       (defun lsp-bridge-jump ()
;;         (interactive)
;;         (cond
;;          ((eq major-mode 'emacs-lisp-mode)
;;           (let ((symb (function-called-at-point)))
;;             (when symb
;;               (find-function symb))))
;;          (lsp-bridge-mode
;;           (lsp-bridge-find-def))
;;          (t
;;           (require 'dumb-jump)
;;           (dumb-jump-go))))

;;       (defun lsp-bridge-jump-back ()
;;         (interactive)
;;         (cond
;;          (lsp-bridge-mode
;;           (lsp-bridge-find-def-return))
;;          (t
;;           (require 'dumb-jump)
;;           (dumb-jump-back))))
;;       )
;;     )
;;   )

;; It shouldn't be installed with quelpa, use git clone /path/to/acm-terminal instead.
;; (defun my-eaf/init-acm-terminal ()
;;   (use-package acm-terminal
;;     :if (not (display-graphic-p))
;;     :load-path (expand-file-name "acm-terminal" (configuration-layer/get-layer-local-dir 'my-eaf))
;;     :after lsp-bridge
;;     :init
;;     (with-eval-after-load 'acm
;;       (require 'acm-terminal))
;;     )
;;   )

(defun my-eaf/init-fuz ()
  (use-package fuz
    :if (my-eaf-fuz-toggle)
    ;; :defer t
    :config
    (unless (require 'fuz-core nil t)
      (fuz-build-and-load-dymod))))

(defun my-eaf/init-helm-fuz ()
  (use-package helm-fuz
    :if (my-eaf-fuz-toggle)
    ;; :diminish helm-fuz-mode " ⓩ"
    :after (fuz helm)
    :config
    (progn
      (helm-fuz-mode)
      (spacemacs|diminish helm-fuz-mode " ⓩ" " z")
      )
    )
  )

(defun my-eaf/init-netease-cloud-music ()
  (use-package netease-cloud-music
    :defer t
    :init
    (setq netease-cloud-music-cache-directory
          (expand-file-name "netease-cloud-music" spacemacs-cache-directory))
    :config
    (progn
      (require 'netease-cloud-music-ui)
      (require 'netease-cloud-music-comment))
    ))

(defun my-eaf/init-snails ()
  (use-package snails
    ;; :if (my-eaf-snails-toggle)
    :config
    (setq snails-show-with-frame nil)
    (add-to-list 'evil-emacs-state-modes 'snails-mode)
    ))


;;; packages.el ends here
