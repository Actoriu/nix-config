{
  config,
  lib,
  ...
}:

with lib;

let
  cfg = config.custom.shell.dircolors;
in
{
  options.custom.shell.dircolors = {
    enable = mkEnableOption "Enable support for dircolors.";
  };

  config = mkIf cfg.enable {
    programs = {
      dircolors = {
        enable = cfg.enable;
      };
    };
  };
}
