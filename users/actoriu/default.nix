{ ... }: {
  imports = [
    ../modules
  ];

  programs.home-manager.enable = true;

  custom = {
    # development
    # cc.enable = true;
    # javascript.enable = true;
    # python.enable = true;
    # texlive.enable = true;
    # editors
    emacs = {
      enable = true;
      spacemacs = true;
      emacs-application-framework = true;
    };
    neovim.enable = true;
    # readers
    zathura.enable = true;
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
    rofi.enable = true;
    tmux.enable = true;
    xdg.enable = true;
    xresources.enable = true;
    zoxide.enable = true;
    zsh.enable = true;
    # terminal
    alacritty.enable = true;
    urxvt.enable = true;
    xst.enable = true;
    # video
    mpv.enable = true;
  };

  xdg.configFile."nixpkgs/config.nix".text = ''
    { ... }: {
      allowUnfree = true;
      allowBroken = true;
      allowUnsupportedSystem = true;
    }
  '';
}
