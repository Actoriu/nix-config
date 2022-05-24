{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.custom.terminal.urxvt;
in
{
  options.custom.terminal.urxvt = {
    enable = mkEnableOption "Enable support for rxvt-unicode terminal emulator.";
  };

  config = mkIf cfg.enable {
    programs = {
      urxvt = {
        enable = cfg.enable;
        package = pkgs.rxvt-unicode;
      };
    };
  };
}
