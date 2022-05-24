{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.custom.hardware.opengl;
in {
  options.custom.hardware.opengl = {
    enable = mkEnableOption "Enable support for the graphical interface nvidia drives.";

    driSupport32Bit = mkEnableOption ''
      Enable support for Enable support for whether to include
      the 32-bit opengl libraries in the system.
    '';
  };

  config = mkIf cfg.enable {
    hardware.opengl.driSupport32Bit = cfg.enable;
  };
}
