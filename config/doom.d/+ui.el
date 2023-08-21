;;; $DOOMDIR/+ui.el -*- lexical-binding: t; -*-


;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq evil-emacs-state-cursor `(box ,(doom-color 'violet)))

(when (display-graphic-p)
  (setq user-font
        (cond
         ((find-font (font-spec :name "Sarasa Mono SC")) "Sarasa Mono SC")
         ((find-font (font-spec :name "OperatorMono Nerd Font")) "OperatorMono Nerd Font")
         ((find-font (font-spec :name "Droid Sans Mono")) "Droid Sans Mono")
         ((find-font (font-spec :name "Droid Sans Fallback")) "Droid Sans Fallback")))

  ;; calculate the font size based on display-pixel-height
  (setq resolution-factor (eval (/ (x-display-pixel-height) 1080.0)))
  (setq doom-font (font-spec :family user-font :size (eval (round (* 16 resolution-factor))))
        doom-big-font (font-spec :family user-font :size (eval (round (* 18 resolution-factor))))
        doom-variable-pitch-font (font-spec :family user-font :size (eval (round (* 16 resolution-factor))))
        doom-modeline-height (eval (round (* 24 resolution-factor))))
  (setq doom-font-increment 1)

  ;; set initl screen size
  ;; (setq initial-frame-alist
  ;;     '((width . 110)
  ;;           (height . 65))))

  (add-hook! 'doom-first-buffer-hook
    (defun +my/change-cjk-font ()
      "change the cjk font and its size to align the org/markdown tables when have
cjk characters. Font should be twice the width of asci chars so that org tables align.
This will break if run in terminal mode, so use conditional to only run for GUI."
      (when (display-graphic-p)
        (setq user-cjk-font
              (cond
               ((find-font (font-spec :name "Hiragino Sans GB")) "Hiragino Sans GB") ; for macos
               ((find-font (font-spec :name "Sarasa Mono SC")) "Sarasa Mono SC") ; for linux
               ))
        (dolist (charset '(kana han cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset (font-spec :family user-cjk-font
                                               :size (eval (round (* 16 resolution-factor)))))))))

  ;; Update window divider in terminal
  ;; https://www.reddit.com/r/emacs/comments/3u0d0u/how_do_i_make_the_vertical_window_divider_more/
  (unless (display-graphic-p)
    (setq evil-insert-state-cursor 'box)
    (defun my-change-window-divider ()
      (ignore-errors
        (let ((display-table (or buffer-display-table standard-display-table)))
          (set-display-table-slot display-table 5 ?│)
          ;; (set-window-display-table (selected-window) display-table)
          )))
    (add-hook 'window-configuration-change-hook #'my-change-window-divider))
