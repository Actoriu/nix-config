{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.private.editors.emacs;

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
  options.private.editors.emacs = {
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
      programs = {
        emacs = {
          enable = cfg.doom-emacs;
          package =
            if pkgs.stdenv.isDarwin
            then pkgs.emacs-macport
            else if pkgs.stdenv.isAarch64
            then pkgs.emacs-nox
            else if (pkgs.stdenv.isLinux && config.private.graphical.display == "wayland")
            then pkgs.emacs-pgtk
            else pkgs.emacs-gtk;
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

      xdg.configFile = {
        "emacs" = {
          source = pkgs.doom-emacs;
          recursive = true;
        };
        "doom" = {
          source = "${cleanSource ../../../config/doom.d}";
          recursive = true;
        };
      };
    })

    (mkIf cfg.spacemacs {
      programs = {
        emacs = {
          enable = cfg.spacemacs;
          package =
            if pkgs.stdenv.isDarwin
            then pkgs.emacs-macport
            else if pkgs.stdenv.isAarch64
            then pkgs.emacs-nox
            else if (pkgs.stdenv.isLinux && config.private.graphical.display == "wayland")
            then pkgs.emacs-pgtk
            else pkgs.emacs-gtk;
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
        emacsPackage =
          if pkgs.stdenv.isDarwin
          then pkgs.emacs-macport
          else if pkgs.stdenv.isAarch64
          then pkgs.emacs-nox
          else if (pkgs.stdenv.isLinux && config.private.graphical.display == "wayland")
          then pkgs.emacs-pgtk
          else pkgs.emacs-gtk;
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
          (python3.withPackages (ps:
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
