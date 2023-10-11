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
    ../../home-manager/i18n/locale
    ../../home-manager/i18n/input-method/fcitx5
    # ../../home-manager/lang/cc
    ../../home-manager/lang/javascript
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
    ../../home-manager/programs/npm
    ../../home-manager/programs/pip
    # ../../home-manager/programs/proxy/geph
    ../../home-manager/programs/rofi
    ../../home-manager/programs/ssh
    ../../home-manager/programs/starship
    # ../../home-manager/programs/urxvt
    # ../../home-manager/programs/tmux
    ../../home-manager/programs/zathura
    ../../home-manager/programs/zoxide
    ../../home-manager/programs/zsh
    ../../home-manager/services/redshift
    ../../home-manager/tools/nix/cachix
  ];

  private = {
    editors = {
      emacs = {
        enable = true;
        doom-emacs = false;
        emacs-application-framework = false;
        nix-doom-emacs = true;
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
