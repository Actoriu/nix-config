{ config
, lib
, ...
}:

with lib;

let
  cfg = config.custom.shell.password-store;
in
{
  options.custom.shell.password-store = {
    enable = mkEnableOption "Enable support for password-store.";
  };

  config = mkIf cfg.enable {
    programs = {
      password-store = {
        enable = cfg.enable;
        # settings = {
        # };
      };
    };
  };
}
