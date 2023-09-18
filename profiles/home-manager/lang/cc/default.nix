{
  config,
  lib,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    # bear
    ccls
    # clang
    cmake
    # gcc
    gdb
    xmake
  ];
}
