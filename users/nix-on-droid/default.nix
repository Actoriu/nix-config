{ ... }: {
  imports = [
    ../../modules/users
  ];

  custom = {
    # development
    cc.enable = true;
    javascript.enable = true;
    python.enable = true;
    # editors
    emacs = {
      enable = true;
      spacemacs = true;
      emacs-application-framework = true;
    };
    neovim.enable = true;
    # readers
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
    tmux.enable = true;
    xdg.enable = true;
    xresources.enable = true;
    zoxide.enable = true;
    zsh.enable = true;
  };

  xdg.configFile."nixpkgs/config.nix".text = "{ ... }: { allowUnfree = true; }";
}
