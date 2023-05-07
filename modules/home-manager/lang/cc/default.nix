{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.custom.cc;
in {
  options.custom.cc = {
    enable = mkEnableOption "Enable support for C/C++ language.";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      # bear
      ccls
      # clang
      cmake
      # gcc
      gdb
    ];
  };
}
