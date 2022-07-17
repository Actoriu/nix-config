{ config
, lib
, ...
}:

with lib;

let
  cfg = config.custom.hardware.cpu.amd;
in
{
  options.custom.hardware.cpu.amd = {
    enable = mkEnableOption "Enable support for amd cpu.";
  };

  config = mkIf cfg.enable {
    hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
}
