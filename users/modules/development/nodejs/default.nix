{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.custom.development.nodejs;
in
{
  options.custom.development.nodejs = {
    enable = mkEnableOption "Enable support for javascript language.";
  };

  config = mkIf cfg.enable {
    home = {
      packages = with pkgs; [
        nodejs
        nodePackages.pyright
      ];
    };
  };
}
