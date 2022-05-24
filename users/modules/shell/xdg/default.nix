{
  config,
  lib,
  ...
}:

with lib;

let
  cfg = config.custom.shell.xdg;
in
{
  options.custom.shell.xdg = {
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
