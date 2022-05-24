{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.custom.development.texlive;
in
{
  options.custom.development.texlive = {
    enable = mkEnableOption "Enable support for texlive language.";
  };

  config = mkIf cfg.enable {
    programs = {
      texlive = {
        enable = true;
      };
    };
  };
}
