{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.custom.password-store;
in {
  options.custom.password-store = {
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
