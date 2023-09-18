{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    nodejs_20
    nodePackages.pyright
  ];
}
