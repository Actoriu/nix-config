;;; funcs.el --- Wanderlust functions File for Spacemacs
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

;;; persp

(defun spacemacs//wanderlust-persp-filter-save-buffers-function (buffer)
  "Filter for rcirc layout."
  (with-current-buffer buffer
    (eq major-mode 'wanderlust-modes)))

(defun spacemacs//wanderlust-buffer-to-persp ()
  "Add buffer to rcirc layout."
  (persp-add-buffer (current-buffer)
                    (persp-get-by-name wanderlust-spacemacs-layout-name)))

;; Customization

(defun user--wanderlust-notify-hook ()
  "Wanderlust email notification hook."
  (cond
   ((featurep 'alert)
    (alert "You've got mail." :severity 'trivial))
   (t (ding))))

(defun wl-summary-overview-entity-compare-by-reply-date (a b)
  "Compare message A and B by latest date of replies including thread."
  (cl-flet ((string-max2 (x y)
                         (cond ((string< x y) y)
                               ('t x)))
            (elmo-entity-to-number (x)
                                   (elt (cddr x) 0))
            (thread-number-get-date (x)
                                    (timezone-make-date-sortable
                                     (elmo-msgdb-overview-entity-get-date
                                      (elmo-message-entity
                                       wl-summary-buffer-elmo-folder x))))
            (thread-get-family (x)
                               (cons x (wl-thread-entity-get-descendant
                                        (wl-thread-get-entity x))))
            (max-reply-date (x)
                            (cond ((eq 'nil x)
                                   'nil)
                                  ((eq 'nil (cdr x))
                                   (thread-number-get-date (car x)))
                                  ('t
                                   (string-max2 (thread-number-get-date (car x))
                                                (max-reply-date (cdr x)))))))
    (string<
     (max-reply-date (thread-get-family (elmo-message-entity-number a)))
     (max-reply-date (thread-get-family (elmo-message-entity-number b))))))


;;; funcs.el ends here
