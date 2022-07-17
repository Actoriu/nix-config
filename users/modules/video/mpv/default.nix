{ config
, lib
, pkgs
, ...
}:

with lib;

let
  cfg = config.custom.video.mpv;
in
{
  options.custom.video.mpv = {
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
