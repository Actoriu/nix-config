{ config
, lib
, ...
}:

with lib;

let
  cfg = config.custom.bat;
in
{
  options.custom.bat = {
    enable = mkEnableOption "Enable support for bat.";
  };

  config = mkIf cfg.enable {
    programs = {
      bat = {
        enable = cfg.enable;
        config = {
          theme = "OneHalfDark";
        };
      };
    };
  };
}
