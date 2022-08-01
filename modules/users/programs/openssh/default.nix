{ config
, lib
, ...
}:

with lib;

let
  cfg = config.custom.openssh;
in
{
  options.custom.openssh = {
    enable = mkEnableOption "Enable support for openssh.";
  };

  config = mkIf cfg.enable {
    programs = {
      ssh = {
        enable = cfg.enable;
      };
    };
  };
}
