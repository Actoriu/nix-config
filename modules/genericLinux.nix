{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.private.home.genericLinux;
in {
  options.private.home.genericLinux = {
    enable = mkEnableOption "Whether to enable settings that make Home Manager work better on
        GNU/Linux distributions other than NixOS.";
  };

  config = mkIf (cfg.enable && pkgs.stdenv.isLinux && config.private.platform == "Home-manager") {
    targets.genericLinux.enable = cfg.enable;
    xsession.enable = cfg.enable;
  };
}
