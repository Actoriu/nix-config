{
  config,
  lib,
  pkgs,
  ...
}: {
  targets.genericLinux.enable = true;
  xsession.enable = true;
}
