{ config
, lib
, ...
}:
with lib; let
  cfg = config.custom.cpu.intel;
in
{
  options.custom.cpu.intel = {
    enable = mkEnableOption "Enable support for intel cpu.";
  };

  config = mkIf cfg.enable {
    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
}
