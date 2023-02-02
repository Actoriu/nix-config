{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.custom.powerManagement;
in {
  options.custom.powerManagement = {
    enable = mkEnableOption "Enable support for power management.";
    acpid = mkEnableOption ''
      Whether to enable the ACPI daemon.
    '';
    powertop = mkEnableOption ''
      Whether to enable powertop auto tuning on startup.
    '';
    tlp = mkEnableOption ''
      Whether to enable the TLP power management daemon.
    '';
    upower = mkEnableOption ''
      Whether to enable Upower, a DBus service that provides power management support to applications.
    '';
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf cfg.powertop {
      services.acpid.enable = cfg.acpid;
    })
    (mkIf cfg.powertop {
      powerManagement.powertop.enable = cfg.powertop;
    })
    (mkIf cfg.tlp {
      services.tlp.enable = cfg.tlp;
    })
    (mkIf cfg.upower {
      services.upower.enable = cfg.upower;
    })
  ]);
}
