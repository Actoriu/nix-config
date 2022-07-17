{ config
, lib
, pkgs
, ...
}:

with lib;

let
  cfg = config.custom.hardware.printers;
in
{
  options.custom.hardware.printers = {
    enable = mkEnableOption "Enable support for printer.";
  };

  config = mkIf cfg.enable {
    programs.system-config-printer.enable = cfg.enable;
    services.printing.enable = cfg.enable;
  };
}
