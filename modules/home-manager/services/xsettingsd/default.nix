{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.custom.xsettingsd;
in {
  options.custom.xsettingsd = {
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
