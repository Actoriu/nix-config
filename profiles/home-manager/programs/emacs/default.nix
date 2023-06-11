{
  config,
  lib,
  pkgs,
  ...
}: {
  programs = {
    emacs = {
      enable = true;
      package =
        if pkgs.stdenv.isDarwin
        then pkgs.emacsMacport
        else if pkgs.stdenv.isAarch64
        then pkgs.emacs-nox
        else pkgs.emacs;
      # extraPackages = epkgs: with epkgs; [
      #   evil
      #   helm
      #   general
      #   magit
      #   nix-mode
      #   company
      # ];
    };
  };

  home = {
    packages = with pkgs; [
      emacs-all-the-icons-fonts
      guile_3_0
      ripgrep
      ugrep
      translate-shell
    ];
  };
}
