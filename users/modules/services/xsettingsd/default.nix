{ config
, lib
, ...
}:

with lib;

let
  cfg = config.custom.services.xsettingsd;
in
{
  options.custom.services.xsettingsd = {
    enable = mkEnableOption "Enable support for xsettingsd.";
  };

  config = mkIf cfg.enable {
    services = {
      xsettingsd = {
        enable = true;
        # settings = {};
      };
    };
  };
}
