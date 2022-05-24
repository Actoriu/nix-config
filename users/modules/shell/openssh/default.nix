{
  config,
  lib,
  ...
}:

with lib;

let
  cfg = config.custom.shell.openssh;
in
{
  options.custom.shell.openssh = {
    enable = mkEnableOption "Enable support for openssh.";
  };

  config = mkIf cfg.enable {
    programs = {
      ssh = {
        enable = cfg.enable;
      };
    };

    # services.openssh = {
    #   enable = cfg.enable;
    # };
  };
}
