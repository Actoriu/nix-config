{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.custom.targets.genericLinux;
in {
  options.custom.targets.genericLinux = {
    enable = mkEnableOption "Whether to enable settings that make Home Manager work better on
        GNU/Linux distributions other than NixOS.";
  };

  config = mkIf (cfg.enable && pkgs.stdenv.isLinux) {
    targets.genericLinux.enable = cfg.enable;
    xsession.enable = cfg.enable;
  };
}
