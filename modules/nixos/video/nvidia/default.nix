{ config
, lib
, pkgs
, ...
}:

with lib;

let
  cfg = config.custom.video.nvidia;
in
{
  options.custom.video.nvidia = {
    enable = mkEnableOption "Enable support for the graphical interface nvidia drives.";

    drivers = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "nvidia-340";
      description = "Enable support for nvidia drives.";
    };
  };

  config = mkIf (cfg.enable && config.services.xserver.enable) (mkMerge [
    {
      boot.blacklistedKernelModules = [ "nouveau" ];

      services.xserver = {
        videoDrivers = [ "nvidia" ];
      };
    }
    (mkIf (cfg.drivers != null && cfg.drivers == "nvidia-340") {
      hardware.nvidia = {
        package = config.boot.kernelPackages.nvidiaPackages.legacy_340;
      };
    })
  ]);
}
