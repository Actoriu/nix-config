{ config
, lib
, ...
}:
with lib; let
  cfg = config.custom.xdg;
in
{
  options.custom.xdg = {
    enable = mkEnableOption "Enable support for xdg.";
  };

  config = mkIf cfg.enable {
    xdg = {
      enable = cfg.enable;
      userDirs = {
        enable = true;
        createDirectories = true;
      };
    };
  };
}
