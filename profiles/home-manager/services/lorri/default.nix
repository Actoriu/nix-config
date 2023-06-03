{
  config,
  lib,
  pkgs,
  ...
}: {
  services.lorri = {
    enable = cfg.enable;
  };
}
