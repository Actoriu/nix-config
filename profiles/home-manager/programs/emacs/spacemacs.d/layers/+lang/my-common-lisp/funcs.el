;;; funcs.el --- Common Lisp functions File for Spacemacs
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

(defun spacemacs//sly-helm-source (&optional table)
  (or table (setq table sly-lisp-implementations))
  `((name . "Sly")
    (candidates . ,(mapcar #'car table))
    (action . (lambda (candidate)
                (car (helm-marked-candidates))))))

(defun spacemacs/helm-sly ()
  (interactive)
  (let ((command (helm :sources (spacemacs//sly-helm-source))))
    (and command (sly (intern command)))))

(when (featurep 'sly)
  (spacemacs|define-transient-state common-lisp-navigation
    :title "Common Lisp Navigation Transient State"
    :doc "
^^Definitions                           ^^Compiler Notes             ^^Stickers
^^^^^^─────────────────────────────────────────────────────────────────────────────────────
[_g_] Jump to definition                [_n_] Next compiler note     [_s_] Next sticker
[_G_] Jump to definition (other window) [_N_] Previous compiler note [_S_] Previous sticker
[_b_] Pop from definition
[_q_] Exit
"
    :foreign-keys run
    :bindings
    ("g" sly-edit-definition)
    ("G" sly-edit-definition-other-window)
    ("b" sly-pop-find-definition-stack)
    ("n" sly-next-note)
    ("N" sly-previous-note)
    ("s" sly-stickers-next-sticker)
    ("S" sly-stickers-prev-sticker)
    ("q" nil :exit t)))

;; Make sure we don't clash with SLY
;; https://github.com/joaotavora/emacsd/blob/master/portacle-slime.el
(defun portacle--resolve-ide-conflict (new-hook
                                       old-hook)
  "Replace OLD-HOOK with NEW-HOOK in `lisp-mode-hook'.
Also re-issue `lisp-mode' in every Lisp source buffer so that SLIME
or SLY are suitably setup there."
  (remove-hook 'lisp-mode-hook old-hook)
  (add-hook 'lisp-mode-hook new-hook t)
  (mapc (lambda (buf)
          (with-current-buffer buf
            (when (eq major-mode 'lisp-mode)
              ;; XXX: actually our own *scratch* is special because
              ;; re-issuing lisp-mode there would drown out the pretty
              ;; buttons.
              (unless (equal "*scratch*" (buffer-name))
                (lisp-mode)))))
        (buffer-list)))


;;; funcs.el ends here
