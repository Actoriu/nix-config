{
  config,
  lib,
  pkgs,
  ...
}: {
  boot.blacklistedKernelModules = ["nouveau"];

  services.xserver = {
    videoDrivers = ["nvidia"];
  };

  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.legacy_340;
  };
}
