{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.custom.lorri;
in {
  options.custom.lorri = {
    enable = mkEnableOption "Enable support for lorri.";
  };

  config = mkIf cfg.enable {
    services.lorri = {
      enable = cfg.enable;
    };
  };
}
