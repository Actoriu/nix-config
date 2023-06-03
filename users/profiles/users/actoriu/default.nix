{
  config,
  lib,
  non-nixos,
  pkgs,
  ...
}: {
  fonts.fontconfig.enable = true;
  i18n.inputMethod.fcitx5 = {
    addons = with pkgs; [
      fcitx5-rime
    ];
  };

  custom = {
    # editors
    emacs = {
      enable = true;
      spacemacs = true;
      emacs-application-framework = true;
      treesitter = true;
    };
    neovim.enable = true;
    # lang
    cc.enable = true;
    javascript.enable = true;
    nix.enable = true;
    # python.enable = true;
    # texlive.enable = true;
    # locale
    # home = {
    #   locale = {
    #     enable = true;
    #     inputMethod = "fcitx5";
    #     locale = "zh_CN";
    #   };
    # };
    # misc
    xdg.enable = true;
    xresources.enable = true;
    # non-nixos
    non-nixos.enable = non-nixos;
    # readers
    zathura.enable = true;
    # services
    redshift.enable = true;
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
    zoxide.enable = true;
    zsh.enable = true;
    # terminal
    # alacritty.enable = false;
    # urxvt.enable = true;
    xst.enable = true;
    # video
    mpv.enable = true;
  };
}
