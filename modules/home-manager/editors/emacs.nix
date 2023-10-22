{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.programs.private.emacs;

  # https://github.com/minimal/dotfiles/blob/master/nixpkgs/emacs.nix#L28
  treeSitterGrammars = pkgs.runCommandLocal "grammars" {} ''
    mkdir -p $out/bin
    ${
      concatStringsSep "\n"
      (mapAttrsToList (name: src: "ln -s ${src}/parser $out/bin/${name}.so") pkgs.tree-sitter.builtGrammars)
    };
  '';

  # list taken from here: https://github.com/emacs-tree-sitter/tree-sitter-langs/tree/master/repos
  # commented out are not yet packaged in nix
  langs = [
    "commonlisp"
    "elisp"
  ];
  grammars = getAttrs (map (lang: "tree-sitter-${lang}") langs) pkgs.tree-sitter.builtGrammars;
in {
  options.programs.private.emacs = {
    enable = mkEnableOption "Enable support for emacs.";
    doom-emacs = mkEnableOption "Enable support for doom-emacs.";
    emacs-application-framework =
      mkEnableOption "Enable support for emacs-application-framework.";
    nix-doom-emacs = mkEnableOption "Enable support for nix-doom-emacs.";
    spacemacs = mkEnableOption "Enable support for spacemacs.";
    treesitter = mkEnableOption "Enable support for tree-sitter.";
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf cfg.doom-emacs {
      /*
      home = {
        sessionPath = ["${config.xdg.configHome}/emacs/bin"];
        sessionVariables = {
          DOOMDIR = "${config.xdg.configHome}/doom";
          DOOMLOCALDIR = "${config.xdg.dataHome}/doom";
          SPACEMACSDIR = "${config.xdg.configHome}/spacemacs.d";
        };
      };
      */
      programs = {
        emacs = {
          enable = cfg.doom-emacs;
          package =
            if pkgs.stdenv.isDarwin
            then pkgs.emacs29-macport
            else if pkgs.stdenv.isAarch64
            then pkgs.emacs29-nox
            else if (pkgs.stdenv.isLinux && config.private.graphical.display == "wayland")
            then pkgs.emacs29-pgtk
            else pkgs.emacs29-gtk3;
          extraPackages = epkgs:
            with epkgs; [
              vterm
            ];
        };
      };

      xdg.configFile = {
        /*
        "chemacs/profiles.el".text = ''
          (("default" . ((user-emacs-directory . "${config.xdg.configHome}/my-emacs")))
           ("doom" . ((user-emacs-directory . "${config.xdg.configHome}/doom-emacs")
                      (env . (("DOOMDIR" . "${config.home.sessionVariables.DOOMDIR}")
                              ("DOOMLOCALDIR" . "${config.home.sessionVariables.DOOMLOCALDIR}")))))
           ("spacemacs" . ((user-emacs-directory . "${config.xdg.configHome}/spacemacs")
                           (env . (("SPACEMACSDIR" . "${config.home.sessionVariables.SPACEMACSDIR}"))))))
        '';
        "chemacs/profile".text = "doom";
        "emacs" = {
          source = pkgs.chemacs2;
          recursive = true;
        };
        */
        /*
        "emacs" = {
          source = pkgs.doom-emacs;
          # recursive = true;
          # onChange = "${pkgs.writeShellScript "doom-change" ''
          #   export DOOMDIR="${config.home.sessionVariables.DOOMDIR}"
          #   export DOOMLOCALDIR="${config.home.sessionVariables.DOOMLOCALDIR}"
          #   if [ ! -d "$DOOMLOCALDIR" ]; then
          #     ${config.xdg.configHome}/doom-emacs/bin/doom --force install
          #   else
          #     ${config.xdg.configHome}/doom-emacs/bin/doom --force clean
          #     ${config.xdg.configHome}/doom-emacs/bin/doom --force sync -u
          #   fi
          # ''}";
        };
        "doom" = {
          # source = "${cleanSource ../../../config/doom.d}";
          source = ../../../config/doom.d;
          # recursive = true;
          # onChange = "${pkgs.writeShellScript "doom-config-packages-change" ''
          #   export DOOMDIR="${config.home.sessionVariables.DOOMDIR}"
          #   export DOOMLOCALDIR="${config.home.sessionVariables.DOOMLOCALDIR}"
          #   ${config.xdg.configHome}/doom-emacs/bin/doom --force sync -u
          # ''}";
        };
        */
        "doom/config.el".text = ''
          ;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

          ;; Place your private configuration here! Remember, you do not need to run 'doom
          ;; sync' after modifying this file!

          ;; Some functionality uses this to identify you, e.g. GPG configuration, email
          ;; clients, file templates and snippets. It is optional.
          (setq user-full-name "Actoriu"
                user-mail-address "evil.actoriu@gmail.com")

          ;; Doom exposes five (optional) variables for controlling fonts in Doom:
          ;;
          ;; - `doom-font' -- the primary font to use
          ;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
          ;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
          ;;   presentations or streaming.
          ;; - `doom-unicode-font' -- for unicode glyphs
          ;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
          ;;
          ;; See 'C-h v doom-font' for documentation and more examples of what they
          ;; accept. For example:
          ;;
          ;; (setq doom-font (font-spec :family "Sarasa Mono SC" :size 16 :weight 'semi-light)
          ;;      doom-variable-pitch-font (font-spec :family "Sarasa Mono SC" :size 17))
          ;;
          ;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
          ;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
          ;; refresh your font settings. If Emacs still can't find your font, it likely
          ;; wasn't installed correctly. Font issues are rarely Doom issues!

          (when (display-graphic-p)
            (setq user-font
                  (cond
                  ((find-font (font-spec :name "Sarasa Mono SC")) "Sarasa Mono SC")
                  ((find-font (font-spec :name "OperatorMono Nerd Font")) "OperatorMono Nerd Font")
                  ((find-font (font-spec :name "Droid Sans Mono")) "Droid Sans Mono")
                  ((find-font (font-spec :name "Droid Sans Fallback")) "Droid Sans Fallback")))

            ;; calculate the font size based on display-pixel-height
            (setq resolution-factor (eval (/ (x-display-pixel-height) 1080.0)))
            (setq doom-font (font-spec :family user-font :size (eval (round (* 27 resolution-factor))))
                  doom-big-font (font-spec :family user-font :size (eval (round (* 32 resolution-factor))))
                  doom-variable-pitch-font (font-spec :family user-font :size (eval (round (* 27 resolution-factor))))
                  doom-modeline-height (eval (round (* 27 resolution-factor))))
            (setq doom-font-increment 1))

          (add-hook! 'doom-first-buffer-hook
            (defun +my/change-cjk-font ()
              "change the cjk font and its size to align the org/markdown tables when have
          cjk characters. Font should be twice the width of asci chars so that org tables align.
          This will break if run in terminal mode, so use conditional to only run for GUI."
              (when (display-graphic-p)
                (setq user-cjk-font
                      (cond
                      ((find-font (font-spec :name "Hiragino Sans GB")) "Hiragino Sans GB") ; for macos
                      ((find-font (font-spec :name "Sarasa Mono SC")) "Sarasa Mono SC") ; for linux
                      ))
                (dolist (charset '(kana han cjk-misc bopomofo))
                  (set-fontset-font (frame-parameter nil 'font)
                                    charset (font-spec :family user-cjk-font
                                                      :size (eval (round (* 27 resolution-factor)))))))))

          ;; There are two ways to load a theme. Both assume the theme is installed and
          ;; available. You can either set `doom-theme' or manually load a theme with the
          ;; `load-theme' function. This is the default:
          (setq doom-theme 'doom-one)

          ;; This determines the style of line numbers in effect. If set to `nil', line
          ;; numbers are disabled. For relative line numbers, set this to `relative'.
          (setq display-line-numbers-type nil)

          ;; If you use `org' and don't want your org files in the default location below,
          ;; change `org-directory'. It must be set before org loads!
          (setq org-directory "~/Documents/org/")

          ;; Whenever you reconfigure a package, make sure to wrap your config in an
          ;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
          ;;
          ;;   (after! PACKAGE
          ;;     (setq x y))
          ;;
          ;; The exceptions to this rule:
          ;;
          ;;   - Setting file/directory variables (like `org-directory')
          ;;   - Setting variables which explicitly tell you to set them before their
          ;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
          ;;   - Setting doom variables (which start with 'doom-' or '+').
          ;;
          ;; Here are some additional functions/macros that will help you configure Doom.
          ;;
          ;; - `load!' for loading external *.el files relative to this one
          ;; - `use-package!' for configuring packages
          ;; - `after!' for running code after a package has loaded
          ;; - `add-load-path!' for adding directories to the `load-path', relative to
          ;;   this file. Emacs searches the `load-path' when you load packages with
          ;;   `require' or `use-package'.
          ;; - `map!' for binding new keys
          ;;
          ;; To get information about any of these functions/macros, move the cursor over
          ;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
          ;; This will open documentation for it, including demos of how they are used.
          ;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
          ;; etc).
          ;;
          ;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
          ;; they are implemented.

          ;; Manually edit .local/custom.el will break doom updates
          (when (file-directory-p custom-file)
            (message (concat "Please delete " custom-file ". And customization in config.el and +ui.el.")))

          ;; tools: rgb
          (add-hook! 'rainbow-mode-hook
            (hl-line-mode (if rainbow-mode -1 +1)))

          ;; editor: format
          (when (executable-find "alejandra")
            (set-formatter! 'alejandra "alejandra --quiet" :modes '(nix-mode)))

          ;; editor: word-wrap (almost) everywhere
          ;; (+global-word-wrap-mode +1)

          ;; Use Tree Sitter wherever we can
          (setq +tree-sitter-hl-enabled-modes t)
          ;; Don't try to download or build the binary, Nix already has it
          (setq tsc-dyn-get-from nil
                tsc-dyn-dir "${pkgs.emacsPackages.tsc}/share/emacs/site-lisp/elpa/${pkgs.emacsPackages.tsc.name}")

          ;;; Tree-sitter support
          ;; https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=emacs-29
          (use-package! treesit
            :when (and (fboundp 'treesit-available-p)
                      (treesit-available-p))
            :custom (major-mode-remap-alist
                    '((c-mode          . c-ts-mode)
                      (c++-mode        . c++-ts-mode)
                      (cmake-mode      . cmake-ts-mode)
                      (conf-toml-mode  . toml-ts-mode)
                      (csharp-mode     . csharp-ts-mode)
                      (css-mode        . css-ts-mode)
                      (java-mode       . java-ts-mode)
                      (js-mode         . js-ts-mode)
                      (js-json-mode    . json-ts-mode)
                      (python-mode     . python-ts-mode)
                      (ruby-mode       . ruby-ts-mode)
                      (rust-mode       . rust-ts-mode)
                      (sh-mode         . bash-ts-mode)
                      (typescript-mode . typescript-ts-mode)
                      ))
            :config
            (add-hook 'markdown-mode-hook #'(lambda () (treesit-parser-create 'markdown)))

            (add-hook 'zig-mode-hook #'(lambda () (treesit-parser-create 'zig)))

            (add-hook 'web-mode-hook #'(lambda ()
                                        (let ((file-name (buffer-file-name)))
                                          (when file-name
                                            (treesit-parser-create
                                              (pcase (file-name-extension file-name)
                                                ("vue" 'vue)
                                                ("html" 'html)
                                                ("php" 'php))))
                                          )))

            (add-hook 'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
            (add-hook 'ielm-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
            (add-hook 'json-mode-hook #'(lambda () (treesit-parser-create 'json)))
            (add-hook 'go-mode-hook #'(lambda () (treesit-parser-create 'go)))
            (add-hook 'java-mode-hook #'(lambda () (treesit-parser-create 'java)))
            (add-hook 'java-ts-mode-hook #'(lambda () (treesit-parser-create 'java)))
            (add-hook 'php-mode-hook #'(lambda () (treesit-parser-create 'php)))
            (add-hook 'php-ts-mode-hook #'(lambda () (treesit-parser-create 'php)))
            (add-hook 'java-ts-mode-hook #'(lambda () (treesit-parser-create 'java)))
            )

          ;; awesome-tray
          ;; (use-package! awesome-tray
          ;;   :hook (doom-after-init . awesome-tray-mode)
          ;;   :custom
          ;;   (awesome-tray-info-padding-right 1)
          ;;   (awesome-tray-date-format "%F %R %a")
          ;;   (awesome-tray-mode-line-active-color "#A45BAD")
          ;;   ;; (awesome-tray-active-modules '("git"
          ;;   ;;                                "parent-dir"
          ;;   ;;                                "mode-name"
          ;;   ;;                                "buffer-name"
          ;;   ;;                                "buffer-read-only"
          ;;   ;;                                "location"))
          ;;   :config
          ;;   ;; Prevent flash of unstyled modeline at startup
          ;;   ;; (unless after-init-time
          ;;   ;;   (awesome-tray-mode 1))
          ;;   (awesome-tray-mode 1)
          ;;   )
        '';
      };
    })

    (mkIf cfg.spacemacs {
      programs = {
        emacs = {
          enable = cfg.spacemacs;
          package =
            if pkgs.stdenv.isDarwin
            then pkgs.emacs29-macport
            else if pkgs.stdenv.isAarch64
            then pkgs.emacs29-nox
            else if (pkgs.stdenv.isLinux && config.private.graphical.display == "wayland")
            then pkgs.emacs29-pgtk
            else pkgs.emacs29-gtk3;
          # extraPackages = epkgs: with epkgs; [
          #   evil
          #   helm
          #   general
          #   magit
          #   nix-mode
          #   company
          # ];
        };
      };

      # home = {
      #   file = {
      #     ".emacs.d" = {
      #       source = pkgs.fetchFromGitHub {
      #         owner = "syl20bnr";
      #         repo = "spacemacs";
      #         rev = "40ae5e2293c6edb5aed1c554ec6b825f24db45d8";
      #         sha256 = "1ki4h1ygzqv95fvck8dmbw7vlkvxzs0qxp0hgfz0d0gcjpx95mdm";
      #       };
      #       recursive = true;
      #     };
      #     ".spacemacs.d" = {
      #       # source = ./init.el;
      #       source = builtins.fetchGit {
      #         url = "https://github.com/Actoriu/spacemacs.d";
      #         ref = "master";
      #       };
      #       recursive = true;
      #     };
      #   };
      # };

      home = {
        file = {
          ".emacs.d" = {
            source = pkgs.spacemacs;
            recursive = true;
          };
          ".spacemacs.d" = {
            source = "${cleanSource ../../../config/spacemacs.d}";
            recursive = true;
          };
        };
      };
    })

    (mkIf cfg.nix-doom-emacs {
      programs.doom-emacs = {
        enable = cfg.nix-doom-emacs;
        doomPrivateDir = ../../../config/doom.d;
        doomPackageDir = pkgs.linkFarm "doom-packages-dir" [
          # straight needs a (possibly empty) `config.el` file to build
          {
            name = "config.el";
            path = pkgs.emptyFile;
          }
          {
            name = "init.el";
            path = ../../../config/doom.d/init.el;
          }
          {
            name = "packages.el";
            path = ../../../config/doom.d/packages.el;
          }
          # { name = "modules"; path = ../../../config/doom.d/modules; }
        ];
        emacsPackage =
          if pkgs.stdenv.isDarwin
          then pkgs.emacs29-macport
          else if pkgs.stdenv.isAarch64
          then pkgs.emacs29-nox
          else if (pkgs.stdenv.isLinux && config.private.graphical.display == "wayland")
          then pkgs.emacs29-pgtk
          else pkgs.emacs-gtk;
        # emacsPackagesOverlay = final: prev: {
        #   ts-fold = prev.ts;
        #   tree-sitter-langs = prev.tree-sitter-langs.override { plugins = pkgs.tree-sitter.allGrammars; };
        # };
      };
    })

    (mkIf (pkgs.stdenv.isAarch64 == false && pkgs.stdenv.isDarwin == false && config.targets.genericLinux == false && cfg.emacs-application-framework) {
      home = {
        packages = with pkgs; [
          # eaf core
          nodejs
          wmctrl
          # eaf-browser
          aria
          qt6.qtwebengine
          # eaf-file-manager
          fd
          # eaf-music-player
          taglib
          (python311.withPackages (ps:
            with ps; [
              # eaf core
              epc
              lxml
              pyqt6
              pyqt6-webengine
              sexpdata
              sip
              tld
              # eaf-browser
              pysocks
              # eaf-airshare
              # eaf-file-browser
              # eaf-file-sender
              qrcode
              # eaf-file-manager
              exif
              pypinyin
              # eaf-git
              charset-normalizer
              pygit2
              pygments
              unidiff
              # eaf-jupyter
              # qtconsole
              # eaf-markdown-previewer
              markdown
              retrying
              # eaf-music-player
              pytaglib
              # eaf-pdf-viewer
              packaging
              pymupdf
              # eaf-system-monitor
              psutil
              # eaf-rss-reader
              pyquery
              feedparser
            ]))
        ];
      };
    })

    (mkIf cfg.treesitter {
      home.file.".tree-sitter".source = pkgs.runCommand "grammars" {} ''
        mkdir -p $out/bin
        ${
          lib.concatStringsSep "\n"
          (lib.mapAttrsToList (name: src: "name=${name}; ln -s ${src}/parser $out/bin/\${name#tree-sitter-}.so") grammars)
        };
      '';
    })
  ]);
}
