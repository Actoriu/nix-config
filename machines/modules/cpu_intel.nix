{ config
, lib
, ...
}:

with lib;

let
  cfg = config.custom.hardware.cpu.intel;
in
{
  options.custom.hardware.cpu.intel = {
    enable = mkEnableOption "Enable support for intel cpu.";
  };

  config = mkIf cfg.enable {
    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
}
