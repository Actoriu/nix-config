{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.custom.emacs;
in {
  options.custom.emacs = {
    enable = mkEnableOption "Enable support for emacs.";
    doom-emacs = mkEnableOption "Enable support for doom-emacs.";
    spacemacs = mkEnableOption "Enable support for spacemacs.";
    emacs-application-framework =
      mkEnableOption "Enable support for emacs-application-framework.";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      programs = {
        emacs = {
          enable = cfg.enable;
          package =
            if pkgs.stdenv.isDarwin
            then pkgs.emacsMacport
            else if pkgs.stdenv.isAarch64
            then pkgs.emacs-nox
            else pkgs.emacs;
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

      home = {
        packages = with pkgs; [
          emacs-all-the-icons-fonts
          guile_3_0
          ripgrep
          ugrep
          translate-shell
        ];
      };
    }

    (mkIf cfg.spacemacs {
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
          # ".spacemacs.d" = {
          #   source = spacemacs.d;
          #   recursive = true;
          # };
        };
      };
    })

    (mkIf (pkgs.stdenv.isAarch64 == false && pkgs.stdenv.isDarwin == false && cfg.emacs-application-framework) {
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
  ]);
}
