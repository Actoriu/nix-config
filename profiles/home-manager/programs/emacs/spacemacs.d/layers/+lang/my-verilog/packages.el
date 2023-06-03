;;; packages.el --- Verilog Layer packages File for Spacemacs
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

(defconst my-verilog-packages
  '(
    company
    evil-matchit
    flycheck
    (verilog-mode :location built-in)
    )
  )

(defun my-verilog/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes verilog-mode))

(defun my-verilog/post-init-evil-matchit ()
  (add-hook 'verilog-mode-hook 'turn-on-evil-matchit-mode))

(defun my-verilog/post-init-flycheck ()
  (spacemacs/enable-flycheck 'verilog-mode)
  (use-package flycheck
    :config
    (progn
      (cond ((and (executable-find "verilator")
                  (executable-find "invoke-verilator"))
             (setq flycheck-verilog-verilator-executable "invoke-verilator")))
      )
    )
  )

(defun my-verilog/init-verilog-mode ()
  (use-package verilog-mode
    :defer t
    :mode (("\\.[ds]?vh?\\'" . verilog-mode)
           ("\\.[st]*v[hp]*\\'" . verilog-mode)
           ("\\.f\\'" . verilog-mode) ;;verilog file lists
           ("\\.psl\\'" . verilog-mode)
           ("\\.vams\\'" . verilog-mode)
           ("\\.vinc\\'" . verilog-mode))
    )
  )


;;; packages.el ends here
