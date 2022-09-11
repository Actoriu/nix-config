{ config
, lib
, ...
}:
with lib; let
  cfg = config.custom.dircolors;
in
{
  options.custom.dircolors = {
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
