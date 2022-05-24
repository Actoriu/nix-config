{
  config,
  lib,
  ...
}:

with lib;

let
  cfg = config.custom.shell.lorri;
in
{
  options.custom.shell.lorri = {
    enable = mkEnableOption "Enable support for lorri.";
  };

  config = mkIf cfg.enable {
    services.lorri = {
      enable = cfg.enable;
    };
  };
}
