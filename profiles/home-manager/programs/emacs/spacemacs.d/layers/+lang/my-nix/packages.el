;;; packages.el --- Nix Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Actoriu <864149939@qq.com>
;; URL: https://github.com/Actoriu/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defconst my-nix-packages
  '(
    company
    (company-nixos-options :requires company)
    flycheck
    (helm-nixos-options :requires helm)
    (nix-mode :location (recipe
                         :fetcher github
                         :repo "NixOS/nix-mode"))
    nixos-options
    nix-sandbox
    nix-update
    ;; nixpkgs-fmt
    ))

(defun my-nix/post-init-company ()
  (add-hook 'nix-mode-hook #'spacemacs//nix-setup-company))

(defun my-nix/init-company-nixos-options ()
  (use-package company-nixos-options
    :if (eq nix-backend 'company-nixos-options)
    :defer t
    :after nixos-options))

(defun my-nix/post-init-flycheck ()
  (spacemacs/enable-flycheck 'nix-mode)
  (use-package flycheck
    :after nix-sandbox
    :init
    (setq flycheck-command-wrapper-function
          (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
          flycheck-executable-find
          (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))
    )
  )

(defun my-nix/init-helm-nixos-options ()
  (use-package helm-nixos-options
    :if (configuration-layer/layer-used-p 'helm)
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "h>" 'helm-nixos-options))))

(defun my-nix/post-nix-mode ()
  (use-package nix-mode
    :mode "\\.nix\\'"
    :init
    (progn
      (spacemacs/register-repl 'nix-mode 'nix-repl-show "nix")
      (add-to-list 'auto-mode-alist '("/flake\\.lock\\'" . json-mode))
      (add-to-list 'spacemacs-indent-sensitive-modes 'nix-mode)
      (add-hook 'nix-mode-hook #'spacemacs//nix-setup-backend)
      )
    :config
    (progn
      (when (executable-find "nixpkgs-fmt")
        (setq nix-nixfmt-bin "nixpkgs-fmt"))
      (electric-indent-mode -1)

      (spacemacs/declare-prefix-for-mode 'nix-mode "ms" "repl")
      (spacemacs/set-leader-keys-for-major-mode 'nix-mode
        "'"  'nix-repl
        "si" 'nix-repl
        "sr" 'nix-repl-show
        )
      )
    )
  )

(defun my-nix/init-nixos-options ()
  (use-package nixos-options
    :defer t))

(defun my-nix/init-nix-sandbox ()
  (use-package nix-sandbox))

(defun my-nix/init-nix-update ()
  (use-package nix-update
    :commands (nix-update-fetch)
    )
  )

;; (defun my-nix/init-nixpkgs-fmt ()
;;   (use-package nixpkgs-fmt
;;     :if (executable-find "nixpkgs-fmt")
;;     :hook (nix-mode . nixpkgs-fmt-on-save-mode)
;;     )
;;   )


;;; packages.el ends here
