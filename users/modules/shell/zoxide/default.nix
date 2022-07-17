{ config
, lib
, ...
}:

with lib;

let
  cfg = config.custom.shell.zoxide;
in
{
  options.custom.shell.zoxide = {
    enable = mkEnableOption "Enable support for zoxide.";
  };

  config = mkIf cfg.enable {
    programs = {
      zoxide = {
        enable = cfg.enable;
      };
    };
  };
}
