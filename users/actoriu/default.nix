{ ... }: {
  imports = [ ../modules ];

  programs.home-manager.enable = true;

  custom = {
    development = {
      cc.enable = true;
      nodejs.enable = true;
      python.enable = true;
      texlive.enable = true;
    };
    editors = {
      emacs = {
        enable = true;
        spacemacs = true;
        emacs-application-framework = true;
      };
      neovim.enable = true;
      zathura.enable = true;
    };
    shell = {
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
    };
    terminal = {
      alacritty.enable = true;
      urxvt.enable = true;
      xst.enable = true;
    };
    # user.name = "actoriu";
    # version.enable = "22.05";
    video = {
      mpv.enable = true;
    };
  };
}
