{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.custom.zoxide;
in {
  options.custom.zoxide = {
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
