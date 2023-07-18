{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    geph.gui
  ];
}
