{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.system-config-printer.enable = true;
  services.printing.enable = true;
}
