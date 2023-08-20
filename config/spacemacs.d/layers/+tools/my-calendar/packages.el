;;; packages.el --- Calendar Layer packages File for Spacemacs
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

(defconst my-calendar-packages
  '(
    alert
    (appt :location built-in)
    )
  )

(defun my-calendar/init-alert ()
  (use-package alert
    :config
    (progn
      (setq alert-default-style 'libnotify
            alert-reveal-idle-time 15
            alert-persist-idle-time 900)
      )
    )
  )

(defun my-calendar/init-appt ()
  (use-package appt
    :config
    (progn
      ;; (appt-activate 1)
      (setq appt-audible t
            appt-display-mode-line t
            appt-issue-message t
            appt-disp-window-function
            (lambda (min-to-app new-time appt-msg)
              (alert :title "appt" (format "\"距約會 %s 還有 %s 分鍾\"" appt-msg min-to-app))
              (appt-disp-window min-to-app new-time appt-msg)))
      )
    )
  )


;;; packages.el ends here
