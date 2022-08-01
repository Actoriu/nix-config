{ config
, lib
, ...
}:

with lib;

let
  cfg = config.custom.network;
in
{
  options.custom.network = {
    enable = mkEnableOption "Enable support for networkmanagement.";
    networkmanager = mkEnableOption "Enable support for networkmanager.";
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf cfg.networkmanager {
      networking.networkmanager.enable = cfg.networkmanager;
    })
  ]);
}
