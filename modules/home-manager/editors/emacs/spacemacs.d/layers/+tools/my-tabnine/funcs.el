;;; funcs.el --- TabNine is the all-language autocompleter functions File for Spacemacs
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

(defun tabnine//merge-company-tabnine-to-company-lsp ()
  (when (memq 'company-lsp company-backends)
    (setq-local company-backends (remove 'company-lsp company-backends))
    (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate)))
  )

(defun tabnine//company-box-icons--tabnine (candidate)
  (when (eq (get-text-property 0 'company-backend candidate)
            'company-tabnine)
    'Reference))

;; Integrate company-tabnine with lsp-mode
(defun company//sort-by-tabnine (candidates)
  (if (or (functionp company-backend)
          (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
      candidates
    (let ((candidates-table (make-hash-table :test #'equal))
          candidates-lsp
          candidates-tabnine)
      (dolist (candidate candidates)
        (if (eq (get-text-property 0 'company-backend candidate)
                'company-tabnine)
            (unless (gethash candidate candidates-table)
              (push candidate candidates-tabnine))
          (push candidate candidates-lsp)
          (puthash candidate t candidates-table)))
      (setq candidates-lsp (nreverse candidates-lsp))
      (setq candidates-tabnine (nreverse candidates-tabnine))
      (nconc (seq-take candidates-tabnine 3)
             (seq-take candidates-lsp 6)))))


;;; funcs.el ends here
