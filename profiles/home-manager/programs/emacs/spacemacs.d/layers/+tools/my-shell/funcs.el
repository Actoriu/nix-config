;;; funcs.el --- Shell Layer functions File for Spacemacs
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

(defun spacemacs//shell-backend ()
  "Returns selected backend."
  (if shell-backend
      shell-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-native-complete))))

(defun spacemacs//shell-setup-backend ()
  "Conditionally setup shell-mode backend."
  (pcase (spacemacs//shell-backend)
    (`lsp (spacemacs//shell-setup-lsp))
    (`company-native-complete (spacemacs//shell-setup-native-complete))))

(defun spacemacs//shell-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (spacemacs//shell-backend)
    (`lsp nil) ;; nothing to do, auto-configured by lsp-mode
    (`company-native-complete (spacemacs//shell-setup-company-native-complete))))

(defun spacemacs//shell-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (lsp)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//shell-setup-native-complete ()
  "Setup company-native-complete backend."
  ;; hook `completion-at-point', optional
  (add-hook 'completion-at-point-functions #'native-complete-at-point nil t)

  ;; (setq completion-at-point-functions '(native-complete-at-point))
  )

(defun spacemacs//shell-setup-company-native-complete ()
  "Setup company-native-complete auto-completion."
  (spacemacs|add-company-backends
    :backends company-native-complete
    :modes shell-mode))


;;; funcs.el ends here
