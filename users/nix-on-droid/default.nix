{...}: {
  custom = {
    # development
    # cc.enable = true;
    # javascript.enable = true;
    # python.enable = true;
    # editors
    emacs = {
      enable = true;
      spacemacs = true;
    };
    # neovim.enable = true;
    # shell
    bat.enable = true;
    dircolors.enable = true;
    direnv = {
      enable = true;
      nix-direnv = true;
    };
    fzf.enable = true;
    git.enable = true;
    gnupg.enable = true;
    openssh.enable = true;
    password-store.enable = true;
    xdg.enable = true;
    zoxide.enable = true;
    zsh.enable = true;
  };
}
