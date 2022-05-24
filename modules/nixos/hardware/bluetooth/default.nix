{
  config,
  lib,
  ...
}:

with lib;

let
  cfg = config.custom.hardware.bluetooth;
in {
  options.custom.hardware.bluetooth = {
    enable = mkEnableOption "Enable support for bluetooth.";
  };

  config = mkIf cfg.enable {
    hardware.bluetooth.enable = cfg.enable;
  };
}
