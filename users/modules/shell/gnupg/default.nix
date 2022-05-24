{
  config,
  lib,
  ...
}:

with lib;

let
  cfg = config.custom.shell.gnupg;
in
{
  options.custom.shell.gnupg = {
    enable = mkEnableOption "Enable support for gnupg.";
  };

  config = mkIf cfg.enable {
    programs = {
      gpg = {
        enable = cfg.enable;
        # settings = {
        # };
      };
    };
  };
}
