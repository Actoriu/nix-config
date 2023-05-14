;;; packages.el --- TabNine is the all-language autocompleter Layer packages File for Spacemacs
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

(defconst my-tabnine-packages
  '(
    company
    company-box
    company-tabnine
    lsp-mode
    )
  )

(defun my-tabnine/post-init-company ()
  (unless (configuration-layer/layer-used-p 'lsp)
    (with-eval-after-load 'company
      (add-to-list 'company-backends #'company-tabnine))))

(defun my-tabnine/post-init-company-box ()
  (spacemacs|use-package-add-hook company-box
    :post-config
    (progn
      (push #'tabnine//company-box-icons--tabnine
            company-box-icons-functions)
      (map-put company-box-backends-colors
               'company-tabnine '(:all
                                  tabnine-company-box-backend-tabnine-face
                                  :selected
                                  tabnine-company-box-backend-tabnine-selected-face)))))

(defun my-tabnine/init-company-tabnine ()
  (use-package company-tabnine
    :defer t
    :commands (company-tabnine)
    :init
    (setq company-tabnine-binaries-folder (concat spacemacs-cache-directory "TabNine"))
    :hook (kill-emacs . company-tabnine-kill-process)
    ;; (lsp-after-open . (lambda ()
    ;;                     (setq company-tabnine-max-num-results 3)
    ;;                     (add-to-list 'company-transformers 'company//sort-by-tabnine t)
    ;;                     (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate))))
    :config
    (progn
      (setq company-tabnine-max-num-results 3)
      (add-to-list 'company-transformers 'company//sort-by-tabnine t)
      ;; The free version of TabNine is good enough,
      ;; and below code is recommended that TabNine not always
      ;; prompt me to purchase a paid version in a large project.
      (defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
        (let ((company-message-func (ad-get-arg 0)))
          (when (and company-message-func
                     (stringp (funcall company-message-func)))
            (unless (string-match "The free version of TabNine only indexes up to"
                                  (funcall company-message-func)) ad-do-it)))))))

(defun my-tabnine/post-init-lsp-mode ()
  (with-eval-after-load 'lsp-mode
    (advice-add 'lsp :after #'tabnine//merge-company-tabnine-to-company-lsp)))


;;; packages.el ends here
