;;; layers.el --- Customized distribution Layer layers File for Spacemacs
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

(configuration-layer/declare-layers '(
                                      ;; my-auto-completion
                                      my-c-c++
                                      (my-common-lisp :variables
                                                      common-lisp-enable-slime t
                                                      common-lisp-enable-local-hyperspec-root nil)
                                      ;; my-calendar
                                      (my-chinese :variables
                                                  my-chinese-enable-cnfonts nil
                                                  my-input-method-enable-smart-input-source nil
                                                  my-chinese-enable-evil-pinyin t)
                                      my-cache-path-from-shell
                                      my-dap
                                      my-direnv
                                      ;; (my-display :variables
                                      ;;             my-display-enable-shrface t
                                      ;;             my-display-enable-inherit-org t)
                                      my-eaf
                                      my-ebook
                                      ;; my-elfeed
                                      (my-encryption :variables
                                                     file-enable-epa-support t
                                                     org-enable-pinentry-support t)
                                      ;; my-eww
                                      (my-emms :variables
                                               emms-enable-player-mpv t)
                                      ;; my-env
                                      my-spacemacs-defaults
                                      ;; my-shell-scripts
                                      ;; my-gettext
                                      ;; my-haskell
                                      my-latex
                                      my-lsp
                                      ;; my-mu4e
                                      ;; my-moccur
                                      (my-org :variables
                                              org-enable-encryption t
                                              org-enable-latex-export t
                                              org-enable-valign nil)
                                      (my-proxy :variables
                                                my-proxy-enable t)
                                      ;; my-pyim
                                      ;; my-rust
                                      ;; my-sawfish
                                      ;; (my-scad :variables
                                      ;;          my-scad-mode-enable t)
                                      my-scheme
                                      ;; (my-shell :variables
                                      ;;           shell-backend 'company-native-complete)
                                      ;; my-tabbar
                                      ;; my-tabnine
                                      ;; (my-translated :variables
                                      ;;                my-translated-enable-sdcv t)
                                      ;; my-tray
                                      my-tree-sitter
                                      ;; my-ui
                                      ;; my-verilog
                                      ;; my-w3m
                                      ;; (my-wanderlust :variables
                                      ;;                my-wanderlust-enable-org t
                                      ;;                my-wanderlust-enable-search-cjk t
                                      ;;                my-wanderlust-enable-w3m-preview t
                                      ;;                my-wanderlust-enable-x-face t
                                      ;;                my-wanderlust-passwd-enable-auth-source t)
                                      )
                                    )


;;; layers.el ends here
