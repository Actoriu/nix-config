;;; packages.el --- Spacemacs Defaults Layer packages File for Spacemacs
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

(defconst my-spacemacs-defaults-packages
  '(
    (dired :location built-in)
    (dired-x :location built-in)
    (dired-aux :location built-in)
    (diredfl :requires dired)
    dired-rsync
    (wdired :location built-in)
    ))

(defun my-spacemacs-defaults/post-init-dired ()
  :init
  (setq dired-auto-revert-buffer t
        dired-dwim-target t
        ;; Always copy/delete recursively
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        delete-by-moving-to-trash t
        image-dired-dir (concat spacemacs-cache-directory "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        ;; Screens are larger nowadays, we can afford slightly larger thumbnails
        image-dired-thumb-size 150)
  :config
  (let ((args (list "-ahl" "-v" "--group-directories-first")))
    (when (spacemacs/system-is-mac)
      ;; Use GNU ls as `gls' from `coreutils' if available. Add `(setq
      ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning
      ;; when not using GNU ls.
      (if-let (gls (executable-find "gls"))
          (setq insert-directory-program gls)
        ;; BSD ls doesn't support -v or --group-directories-first
        (setq args (list (car args)))))
    (setq dired-listing-switches (string-join args " "))

    (add-hook 'dired-mode-hook
              (defun +dired-disable-gnu-ls-flags-in-tramp-buffers-h ()
                "Fix #1703: dired over TRAMP displays a blank screen.
This is because there's no guarantee the remote system has GNU ls, which is the
only variant that supports --group-directories-first."
                (when (file-remote-p default-directory)
                  (setq-local dired-actual-switches (car args))))))

  ;; Don't complain about this command being disabled when we use it
  (put 'dired-find-alternate-file 'disabled nil)
  )

(defun my-spacemacs-defaults/pre-init-dired-x ()
  (spacemacs|use-package-add-hook dired-x
    :post-config
    (setq dired-omit-verbose nil
          dired-omit-files
          (concat dired-omit-files
                  "\\|^.DS_Store\\'"
                  "\\|^.project\\(?:ile\\)?\\'"
                  "\\|^.\\(svn\\|git\\)\\'"
                  "\\|^.ccls-cache\\'"
                  "\\|\\(?:\\.js\\)?\\.meta\\'"
                  "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
    ;; Let OS decide how to open certain files
    (when-let (cmd (cond
                    ((spacemacs/system-is-mac) "open")
                    ((spacemacs/system-is-linux) "xdg-open")
                    ((spacemacs/system-is-mswindows) "start")))
      (setq dired-guess-shell-alist-user
            `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
              ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
              ("\\.\\(?:xcf\\)\\'" ,cmd)
              ("\\.csv\\'" ,cmd)
              ("\\.tex\\'" ,cmd)
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
              ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
              ("\\.html?\\'" ,cmd)
              ("\\.md\\'" ,cmd))))
    )
  )

(defun my-spacemacs-defaults/init-dired-aux ()
  (use-package dired-aux
    :defer t
    :config
    (setq dired-create-destination-dirs 'ask
          dired-vc-rename-file t)
    )
  )

(defun my-spacemacs-defaults/init-diredfl ()
  (use-package diredfl
    :hook (dired-mode . diredfl-mode)
    )
  )

(defun my-spacemacs-defaults/init-dired-rsync ()
  (use-package dired-rsync
    :bind (:map dired-mode-map
                ("C-c C-r" . dired-rsync))))

(defun my-spacemacs-defaults/init-wdired ()
  (use-package wdired
    :after dired
    :commands wdired-change-to-wdired-mode
    :config
    (progn
      ;; allow to edit permission bits
      (setq wdired-allow-to-change-permissions t
            ;; allow to edit symlinks
            wdired-allow-to-redirect-links nil)
      )
    )
  )


;;; packages.el ends here
