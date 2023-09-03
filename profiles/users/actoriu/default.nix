{
  config,
  inputs,
  lib,
  non-nixos,
  pkgs,
  ...
}: {
  imports = [
    ../../../modules/home-manager
    ../../home-manager/i18n/input-method/fcitx5
    ../../home-manager/lang/nix
    ../../home-manager/misc/fontconfig
    ../../home-manager/misc/gtk
    ../../home-manager/misc/qt
    ../../home-manager/misc/xdg
    ../../home-manager/misc/xresources
    ../../home-manager/programs/bat
    ../../home-manager/programs/dircolors
    ../../home-manager/programs/direnv
    ../../home-manager/programs/emacs
    ../../home-manager/programs/fzf
    ../../home-manager/programs/git
    ../../home-manager/programs/gpg
    ../../home-manager/programs/mpv
    ../../home-manager/programs/neovim
    # ../../home-manager/programs/proxy/geph
    ../../home-manager/programs/rofi
    ../../home-manager/programs/ssh
    ../../home-manager/programs/starship
    ../../home-manager/programs/urxvt
    ../../home-manager/programs/tmux
    ../../home-manager/programs/zathura
    ../../home-manager/programs/zoxide
    ../../home-manager/programs/zsh
    ../../home-manager/services/gammastep
    ../../home-manager/tools/nix/cachix
  ];

  i18n.glibcLocales = pkgs.glibcLocales.override {
    allLocales = false;
    locales = ["en_US.UTF-8/UTF-8" "zh_CN.UTF-8/UTF-8"];
  };

  home = {
    language = {
      base = "zh_CN.UTF-8";
    };
  };

  private = {
    editors = {
      emacs = {
        enable = true;
        doom-emacs = true;
        emacs-application-framework = false;
        nix-doom-emacs = false;
        spacemacs = false;
        treesitter = false;
      };
    };
    genericLinux.enable = non-nixos;
    graphical = {
      # enable = true;
      display = "x11";
    };
    terminal = {
      alacritty = {
        enable = true;
      };
    };
  };
}
