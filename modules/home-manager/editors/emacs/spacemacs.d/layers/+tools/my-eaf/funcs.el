;;; funcs.el --- Emacs Application Framework Layer functions File for Spacemacs
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

(defun my-eaf-python-detect ()
  "Check python3."
  (cond
   ((or (executable-find "python3")
        (and (executable-find "python")
             (> (length (shell-command-to-string "python --version | grep 'Python 3'")) 0)))
    t)
   ;; (t (message "Python binary not found."))
   ))

(defun my-eaf-pip-detect ()
  "Check pip3."
  (cond
   ((or (executable-find "pip3")
        (and (executable-find "pip")
             (> (length (shell-command-to-string "pip --version | grep 'python 3'")) 0)))
    t)
   ;; (t (message "Python pip binary not found."))
   ))

(defun my-eaf-detect ()
  "Check basic requirements for EAF to run."
  (cond
   ((and (my-eaf-python-detect)
         (my-eaf-pip-detect)
         (not
          (equal
           (shell-command-to-string "pip freeze | grep '^PyQt\\|PyQtWebEngine'") "")))
    t)
   ;; (t (message "Eaf binary not found."))
   ))

(defun my-eaf-toggle ()
  "Check basic requirements for EAF to run."
  (cond
   ((and (display-graphic-p)
         (my-eaf-detect))
    t)
   ;; (t (message "Eaf render candidates that only can be run in a graphical environment."))
   ))

(defun my-eaf-fuz-detect ()
  "Check fuz."
  (cond
   ((and my-eaf-snails-enable-fuz
         (executable-find "cargo"))
    t)
   ;; (t (message "Rust package manager \"cargo\" not found!"))
   ))

(defun my-eaf-snails-detect ()
  "Check snails."
  (cond
   (my-eaf-snails-enable t)
   ;; (t (message "Snails render candidates that only can be run in a graphical environment."))
   ))

(defun my-eaf-fuz-toggle ()
  "Check basic requirements for Fuz to run."
  (if (my-eaf-fuz-detect) t))

(defun my-eaf-snails-toggle ()
  "Check basic requirements for Snails to run."
  (cond
   ((and (my-eaf-fuz-detect)
         (my-eaf-snails-detect))
    t)
   ))

(defun duckduckgo ()
  (interactive)
  (eaf-open-browser "www.duckduckgo.com"))
(defun wikipedia ()
  (interactive)
  (eaf-open-browser "www.wikipedia.com"))
(defun youtube ()
  (interactive)
  (eaf-open-browser "www.youtube.com"))

(defun eaf-toggle-dark-mode ()
  (interactive)
  (if (not (eq major-mode 'eaf-mode))
      (message "Not in eaf-mode buffer")
    (let* ((app-name eaf--buffer-app-name)
           (app-dark-mode
            (intern
             (format "eaf-%s-dark-mode"
                     (if (string= app-name "pdf-viewer")
                         "pdf"
                       app-name))))
           (current-state (cdr (assoc app-dark-mode eaf-var-list))))
      (let ((browser (string= app-name "browser")))
        (cond ((member current-state '("true" "follow" "ignore"))
               (eaf-set app-dark-mode "false")
               (when browser
                 (eaf-proxy-refresh_page)))
              (t
               (eaf-set app-dark-mode "true")
               (when browser
                 (eaf-proxy-insert_or_dark_mode))))
        (unless browser
          (eaf-restart-process))))))

(defun spacemacs/open-with-eaf ()
  (interactive)
  (eaf-open (buffer-file-name)))

;;; Helm
(defvar helm-bookmark-preds+
  '(helm-bookmark-org-file-p
    helm-bookmark-addressbook-p
    helm-bookmark-gnus-bookmark-p
    helm-bookmark-w3m-bookmark-p
    helm-bookmark-woman-man-bookmark-p
    helm-bookmark-info-bookmark-p
    helm-bookmark-image-bookmark-p
    helm-bookmark-file-p
    helm-bookmark-helm-find-files-p
    helm-bookmark-addressbook-p))

(define-advice helm-bookmark-uncategorized-bookmark-p (:override (bookmark) customize)
  "Return non--nil if BOOKMARK match no known category.
This uses `helm-bookmark-preds+' to make it easier to add categories."
  (cl-loop for pred in helm-bookmark-preds+
           never (funcall pred bookmark)))

(with-eval-after-load 'helm-bookmark
  (push 'helm-bookmark-eaf-p+ helm-bookmark-preds+)

  (defun helm-bookmark-eaf-p+ (bookmark)
    "Check if bookmark is an eaf bookmark."
    (eq (bookmark-get-handler bookmark)
        'eaf--bookmark-restore))

  (dolist (app '("browser" "pdf-viewer"))
    (push
     (helm-make-source (format "Bookmark %s" app) 'helm-source-filtered-bookmarks
       :init (lambda ()
               (bookmark-maybe-load-default-file)
               (helm-init-candidates-in-buffer
                   'global
                 (helm-bookmark-filter-setup-alist
                  (lambda (bookmark)
                    (equal (bookmark-prop-get bookmark 'eaf-app)
                           app))))))
     helm-bookmark-default-filtered-sources)))

;;; Org-mode
(defun eaf-org-open-file (file &optional link)
  "An wrapper function on `eaf-open'."
  (eaf-open file))

;;; LaTeX
;; from TeX-pdf-tools-sync-view
;;;###autoload
(defun TeX-eaf-sync-view()
  (unless (fboundp 'eaf-open)
    (error "EAF is not available!"))
  (let* ((pdf (TeX-active-master (TeX-output-extension)))
         (url (expand-file-name pdf))
         (app-name "pdf-viewer")
         (exists-eaf-buffer)
         (eaf-buffer-window))
    (catch 'found-match-buffer
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'eaf-mode)
          (when (and (string= buffer-url url)
                     (string= buffer-app-name app-name))
            (setq exists-eaf-buffer buffer)
            (setq eaf-buffer-window (get-buffer-window exists-eaf-buffer))
            (throw 'found-match-buffer t)))))
    (if (and exists-eaf-buffer eaf-buffer-window)
        (pop-to-buffer exists-eaf-buffer)
      (eaf-open url app-name "--synctex_on=True"))))

(eval-after-load "tex"
  '(progn
     (add-to-list 'TeX-view-program-list '("eaf" TeX-eaf-sync-view))
     (add-to-list 'TeX-view-program-selection '(output-pdf "eaf"))
     ))


;;; funcs.el ends here
