{ config
, lib
, ...
}:
with lib; let
  cfg = config.custom.gnupg;
in
{
  options.custom.gnupg = {
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
