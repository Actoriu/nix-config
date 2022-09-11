{ config
, lib
, ...
}:
with lib; let
  cfg = config.custom.bluetooth;
in
{
  options.custom.bluetooth = {
    enable = mkEnableOption "Enable support for bluetooth.";
  };

  config = mkIf cfg.enable {
    hardware.bluetooth.enable = cfg.enable;
  };
}
