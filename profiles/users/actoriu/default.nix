{
  config,
  inputs,
  lib,
  non-nixos,
  pkgs,
  ...
}: {
  imports = [
    ../../../modules/home-manager/module-list.nix
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
    # ../../home-manager/programs/emacs
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

  home = {
    language = {
      base = "zh_CN.UTF-8";
    };
  };

  custom = {
    programs = {
      alacritty = {
        enable = true;
      };
      emacs = {
        enable = true;
        emacs-application-framework = true;
        spacemacs = false;
        nix-doom-emacs = true;
        treesitter = false;
      };
    };
    targets.genericLinux.enable = non-nixos;
  };
}
