{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.customize.non-nixos;
in {
  options.customize.non-nixos = {
    enable = mkEnableOption "Whether to enable settings that make Home Manager work better on
        GNU/Linux distributions other than NixOS.";
  };

  config = mkIf (cfg.enable && pkgs.stdenv.isLinux) {
    targets.genericLinux.enable = cfg.enable;
    xsession.enable = cfg.enable;
  };
}
