{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.custom.programs.emacs;

  # https://github.com/minimal/dotfiles/blob/master/nixpkgs/emacs.nix#L28
  treeSitterGrammars = pkgs.runCommandLocal "grammars" {} ''
    mkdir -p $out/bin
    ${
      lib.concatStringsSep "\n"
      (lib.mapAttrsToList (name: src: "ln -s ${src}/parser $out/bin/${name}.so") pkgs.tree-sitter.builtGrammars)
    };
  '';

  # list taken from here: https://github.com/emacs-tree-sitter/tree-sitter-langs/tree/master/repos
  # commented out are not yet packaged in nix
  langs = [
    "commonlisp"
    "elisp"
  ];
  grammars = lib.getAttrs (map (lang: "tree-sitter-${lang}") langs) pkgs.tree-sitter.builtGrammars;
in {
  options.custom.programs.emacs = {
    enable = mkEnableOption "Enable support for emacs.";
    emacs-application-framework =
      mkEnableOption "Enable support for emacs-application-framework.";
    doom-emacs = mkEnableOption "Enable support for doom-emacs.";
    spacemacs = mkEnableOption "Enable support for spacemacs.";
    treesitter = mkEnableOption "Enable support for tree-sitter.";
  };

  config = mkIf cfg.enable (mkMerge [
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
          ".spacemacs.d" = {
            source = "${cleanSource ../../../../profiles/home-manager/programs/emacs/spacemacs.d}";
            recursive = true;
          };
        };
      };
    })

    (mkIf (pkgs.stdenv.isAarch64 == false && pkgs.stdenv.isDarwin == false && config.targets.genericLinux.enable == false && cfg.emacs-application-framework) {
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
