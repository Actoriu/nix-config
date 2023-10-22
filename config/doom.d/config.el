;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Actoriu"
      user-mail-address "evil.actoriu@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; (setq doom-font (font-spec :family "Sarasa Mono SC" :size 16 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Sarasa Mono SC" :size 17))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

(when (display-graphic-p)
  (setq user-font
        (cond
         ((find-font (font-spec :name "Sarasa Mono SC")) "Sarasa Mono SC")
         ((find-font (font-spec :name "OperatorMono Nerd Font")) "OperatorMono Nerd Font")
         ((find-font (font-spec :name "Droid Sans Mono")) "Droid Sans Mono")
         ((find-font (font-spec :name "Droid Sans Fallback")) "Droid Sans Fallback")))

  ;; calculate the font size based on display-pixel-height
  (setq resolution-factor (eval (/ (x-display-pixel-height) 1080.0)))
  (setq doom-font (font-spec :family user-font :size (eval (round (* 27 resolution-factor))))
        doom-big-font (font-spec :family user-font :size (eval (round (* 32 resolution-factor))))
        doom-variable-pitch-font (font-spec :family user-font :size (eval (round (* 27 resolution-factor))))
        doom-modeline-height (eval (round (* 27 resolution-factor))))
  (setq doom-font-increment 1))

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
                                             :size (eval (round (* 27 resolution-factor)))))))))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(when (modulep! :ui doom)
  (setq doom-theme 'doom-one))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(when (modulep! :lang org)
  (setq org-directory "~/Documents/org/"))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; tools: rgb
(when (modulep! :tools rgb)
  (add-hook! 'rainbow-mode-hook
    (hl-line-mode (if rainbow-mode -1 +1))))

;; editor: format
(when (and (modulep! :editor format)
           (executable-find "alejandra"))
  (set-formatter! 'alejandra "alejandra --quiet" :modes '(nix-mode)))

;; editor: word-wrap (almost) everywhere
(when (modulep! :editor word-wrap)
  (+global-word-wrap-mode +1))

;; Use Tree Sitter wherever we can
(when (modulep! :tools tree-sitter)
  (setq +tree-sitter-hl-enabled-modes t
        ;; Don't try to download or build the binary, Nix already has it
        tsc-dyn-get-from nil
        ;; tsc-dyn-dir "${pkgs.emacsPackages.tsc}/share/emacs/site-lisp/elpa/${pkgs.emacsPackages.tsc.name}"
        ))

;;; Tree-sitter support
;; https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=emacs-29
(use-package! treesit-auto
  :when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
  :config
  ;; (setq treesit-auto-install 'prompt)
  (setq treesit-font-lock-level 4)
  (treesit-auto-add-to-auto-mode-alist)
  (global-treesit-auto-mode))

;; awesome-tray
;; (use-package! awesome-tray
;;   :hook (doom-after-init . awesome-tray-mode)
;;   :custom
;;   (awesome-tray-info-padding-right 1)
;;   (awesome-tray-date-format "%F %R %a")
;;   (awesome-tray-mode-line-active-color "#A45BAD")
;;   ;; (awesome-tray-active-modules '("git"
;;   ;;                                "parent-dir"
;;   ;;                                "mode-name"
;;   ;;                                "buffer-name"
;;   ;;                                "buffer-read-only"
;;   ;;                                "location"))
;;   :config
;;   ;; Prevent flash of unstyled modeline at startup
;;   ;; (unless after-init-time
;;   ;;   (awesome-tray-mode 1))
;;   (awesome-tray-mode 1)
;;   )
