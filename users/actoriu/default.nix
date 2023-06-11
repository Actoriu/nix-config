{
  config,
  lib,
  non-nixos,
  pkgs,
  ...
}: {
  imports = [
    ../../modules/home-manager/module-list.nix
    ../../profiles/home-manager/i18n/input-method/fcitx5
    ../../profiles/home-manager/lang/nix
    ../../profiles/home-manager/misc/fontconfig
    ../../profiles/home-manager/misc/xdg
    ../../profiles/home-manager/misc/xresources
    # ../../profiles/home-manager/programs/alacritty
    ../../profiles/home-manager/programs/bat
    ../../profiles/home-manager/programs/dircolors
    ../../profiles/home-manager/programs/direnv
    ../../profiles/home-manager/programs/emacs
    ../../profiles/home-manager/programs/fzf
    ../../profiles/home-manager/programs/git
    ../../profiles/home-manager/programs/gpg
    ../../profiles/home-manager/programs/mpv
    ../../profiles/home-manager/programs/neovim
    ../../profiles/home-manager/programs/rofi
    ../../profiles/home-manager/programs/ssh
    ../../profiles/home-manager/programs/tmux
    ../../profiles/home-manager/programs/urxvt
    ../../profiles/home-manager/programs/zathura
    ../../profiles/home-manager/programs/zoxide
    ../../profiles/home-manager/programs/zsh
    ../../profiles/home-manager/services/redshift
    ../../profiles/home-manager/targets/linux
  ];

  customize = {
    emacs = {
      enable = true;
      emacs-application-framework = false;
      spacemacs = true;
      treesitter = true;
    };
  };
}
