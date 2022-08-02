{ config
, lib
, pkgs
, ...
}:

with lib;

let
  cfg = config.custom.urxvt;
in
{
  options.custom.urxvt = {
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
