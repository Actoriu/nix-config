{ config
, lib
, pkgs
, ...
}:
with lib; let
  cfg = config.custom.texlive;
in
{
  options.custom.texlive = {
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
