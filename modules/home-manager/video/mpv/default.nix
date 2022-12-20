{ config
, lib
, pkgs
, ...
}:
with lib; let
  cfg = config.custom.mpv;
in
{
  options.custom.mpv = {
    enable = mkEnableOption "Enable support for mpv.";
  };

  config = mkIf cfg.enable {
    programs = {
      mpv = {
        enable = cfg.enable;
      };
    };
  };
}
