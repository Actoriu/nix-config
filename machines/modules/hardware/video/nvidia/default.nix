{ config
, lib
, pkgs
, ...
}:

with lib;

let
  cfg = config.custom.hardware.video.nvidia;
in
{
  options.custom.hardware.video.nvidia = {
    enable = mkEnableOption "Enable support for the graphical interface nvidia drives.";

    drivers = mkOption {
      type = types.str;
      default = null;
      example = "nvidia-340";
      description = "Enable support for nvidia drives.";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf (cfg.drivers == "nvidia-340") {
      services.xserver = {
        videoDrivers = [ "nvidia" ];
      };

      hardware.nvidia = {
        package = config.boot.kernelPackages.nvidiaPackages.legacy_340;
      };
    })
    {
      boot.blacklistedKernelModules = [ "nouveau" ];
    }
  ]);
}
