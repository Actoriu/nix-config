{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ../../../modules/home-manager/module-list.nix
    ../../home-manager/misc/xdg
    ../../home-manager/misc/xresources
    ../../home-manager/programs/bat
    ../../home-manager/programs/dircolors
    ../../home-manager/programs/direnv
    ../../home-manager/programs/emacs
    ../../home-manager/programs/fzf
    ../../home-manager/programs/git
    ../../home-manager/programs/gpg
    ../../home-manager/programs/neovim
    ../../home-manager/programs/rofi
    ../../home-manager/programs/ssh
    ../../home-manager/programs/tmux
    ../../home-manager/programs/zoxide
    ../../home-manager/programs/zsh
  ];

  custom = {
    programs = {
      emacs = {
        enable = true;
        emacs-application-framework = false;
        spacemacs = true;
        treesitter = true;
      };
    };
  };
}
