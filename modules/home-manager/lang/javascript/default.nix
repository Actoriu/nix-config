{ config
, lib
, pkgs
, ...
}:
with lib; let
  cfg = config.custom.javascript;
in
{
  options.custom.javascript = {
    enable = mkEnableOption "Enable support for javascript language.";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      nodejs-18_x
      nodePackages.pyright
    ];
    # programs.npm.enable = true;
  };
}
