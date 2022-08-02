{ config
, lib
, pkgs
, ...
}:

with lib;

let
  cfg = config.custom.printers;
in
{
  options.custom.printers = {
    enable = mkEnableOption "Enable support for printer.";
  };

  config = mkIf cfg.enable {
    programs.system-config-printer.enable = cfg.enable;
    services.printing.enable = cfg.enable;
  };
}
