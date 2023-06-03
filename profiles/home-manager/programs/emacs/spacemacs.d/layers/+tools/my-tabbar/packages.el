;;; packages.el --- Tabbar Layer packages File for Spacemacs
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

(defconst my-tabbar-packages
  '(
    (awesome-tab :location (recipe
                            :fetcher github
                            :repo "manateelazycat/awesome-tab"
                            :files ("*"
                                    (:exclude "README.md" "screenshot.png"))))
    ))

(defun my-tabbar/init-awesome-tab ()
  (use-package awesome-tab
    :defer t
    :commands (awesome-tab-mode)
    :init
    (progn
      ;; 高度
      (defface awesome-tab-default
        '(
          (t
           :inherit default
           :height 1.0
           ))
        "Default face used in the tab bar."
        :group 'awesome-tab)

      ;; 选中
      (defface awesome-tab-selected
        '((t (:inherit awesome-tab-default :weight ultra-bold :width semi-expanded
                       :foreground "#A45BAD" :overline "#A45BAD" :underline "#A45BAD")))
        "Face used for the selected tab."
        :group 'awesome-tab)

      ;; 未选中
      (defface awesome-tab-unselected
        '((t
           (:inherit awesome-tab-default
                     :foreground "#5D4D7A" :overline "#5D4D7A" :underline "#5D4D7A")))
        "Face used for unselected tabs."
        :group 'awesome-tab)

      ;; 间隔
      (defface awesome-tab-separator
        '((t
           :inherit awesome-tab-default
           :height 0.1
           :box (:line-width 1 :color "#EEAD0E" :style pressed-button)
           ))
        "Face used for separators between tabs."
        :group 'awesome-tab)

      (spacemacs|define-transient-state awesometab
        :title "Awesome-tab Transient State"
        :doc "
 Fast Move^^^^           Tab^^^^                   Search^^      Misc^^
 ───────^^^^──────────── ─────^^^^──────────────── ──────^^───── ─────^^──────────────
 [_p_/_n_] switch group  [_C-a_/_C-e_] first/last  [_b_] buffer  [_C-k_] kill buffer
 [_h_/_l_] switch tab    [_C-j_]^^ ace jump        [_g_] group   [_C-S-k_] kill others in group
 [_H_/_L_] switch other  [_C-h_/_C-l_] move        ^^            [_q_] quit
"
        :on-enter (awesome-tab-mode t)
        :on-exit (awesome-tab-mode -1)
        :bindings
        ;; Fast Move
        ("p" awesome-tab-backward-group)
        ("n" awesome-tab-forward-group)
        ("h" awesome-tab-backward-tab)
        ("l" awesome-tab-forward-tab)
        ("H" awesome-tab-forward-tab-other-window)
        ("L" awesome-tab-backward-tab-other-window)
        ;; Tab
        ("C-a" awesome-tab-select-beg-tab)
        ("C-e" awesome-tab-select-end-tab)
        ("C-j" awesome-tab-ace-jump)
        ("C-h" awesome-tab-move-current-tab-to-left)
        ("C-l" awesome-tab-move-current-tab-to-right)
        ;; Search
        ("b" ivy-switch-buffer)
        ("g" awesome-tab-counsel-switch-group)
        ;; Misc
        ("C-k" kill-current-buffer)
        ("C-S-k" awesome-tab-kill-other-buffers-in-current-group)
        ("q" nil :exit t))
      (memacs/define-evil-normal-keybinding "C-t" 'spacemacs/awesometab-transient-state/body))
    :config
    (progn
      (set-face-attribute 'awesome-tab-selected nil
                          :foreground awesome-tab-active-color
                          :underline nil
                          :overline awesome-tab-active-color)
      (set-face-attribute 'awesome-tab-unselected nil
                          :foreground awesome-tab-inactive-color
                          :underline awesome-tab-inactive-color
                          :overline nil)
      (setq awesome-tab-cycle-scope 'tabs)
      (awesome-tab-mode 1))
    ))


;;; packages.el ends here
