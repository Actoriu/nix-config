{ config
, lib
, pkgs
, ...
}:

with lib;

let
  cfg = config.custom.version;
in
{
  options.custom.version = {
    enable = mkOption {
      type = types.nullOr (types.enum [ "22.05" "22.11" ]);
      default = null;
      example = "22.05";
      description = "Enable support for stateVersion.";
    };
  };

  config = mkIf (cfg.enable != null) {
    system.stateVersion = cfg.enable;
  };
}
