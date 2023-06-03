{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    nodejs-18_x
    nodePackages.pyright
  ];
  # programs.npm.enable = true;
}
