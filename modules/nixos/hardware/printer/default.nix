{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.custom.hardware.printer;
in {
  options.custom.hardware.printer = {
    enable = mkEnableOption "Enable support for printer.";
  };

  config = mkIf cfg.enable {
    programs.system-config-printer.enable = true;
    services.printing.enable = true;
  };
}
