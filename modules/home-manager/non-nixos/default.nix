{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.customize.home-manager.non-nixos;
in {
  options.customize.home-manager.non-nixos = {
    enable = mkEnableOption "Whether to enable settings that make Home Manager work better on
        GNU/Linux distributions other than NixOS.";
  };

  config = lib.mkIf (cfg.enable && pkgs.stdenv.isLinux) {
    targets.genericLinux.enable = cfg.enable;
    xsession.enable = cfg.enable;
  };
}
