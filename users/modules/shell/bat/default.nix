{ config
, lib
, ...
}:

with lib;

let
  cfg = config.custom.shell.bat;
in
{
  options.custom.shell.bat = {
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
