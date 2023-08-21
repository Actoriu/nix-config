{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [nix-doom-emacs.hmModule];

  programs = {
    doom-emacs = {
      enable = true;
      doomPrivateDir = ../../../../config/doom.d;
      emacsPackage = pkgs.emacs-gtk;
      # package =
      #   if pkgs.stdenv.isDarwin
      #   then pkgs.emacs-macport
      #   else if pkgs.stdenv.isAarch64
      #   then pkgs.emacs-nox
      #   else pkgs.emacs-gtk;
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
