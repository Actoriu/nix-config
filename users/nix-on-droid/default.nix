{ ... }: {
  imports = [ ../modules/default.nix ];

  programs.home-manager.enable = true;
  manual.manpages.enable = false;

  custom = {
    development = {
      # cc.enable = true;
      nodejs.enable = true;
      python.enable = true;
      # texlive.enable = true;
    };
    editors = {
      emacs.enable = true;
      neovim.enable = true;
      # zathura.enable = true;
    };
    # redshift.enable = true;
    # xsettingsd.enable = true;
    shell = {
      bat.enable = true;
      dircolors.enable = true;
      direnv.enable = true;
      fzf.enable = true;
      git.enable = true;
      gnupg.enable = true;
      # lorri.enable = true;
      openssh.enable = true;
      password-store.enable = true;
      # rofi.enable = true;
      # tmux.enable = true;
      xdg.enable = true;
      # xresources.enable = true;
      zoxide.enable = true;
      zsh.enable = true;
    };
    # terminal = {
    #   alacritty.enable = true;
    #   urxvt.enable = true;
    #   xst.enable = true;
    # };
    # video = {
    #   mpv.enable = true;
    # };
  };
}
