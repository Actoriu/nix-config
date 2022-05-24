{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.custom.hardware.pulseaudio;
in {
  options.custom.hardware.pulseaudio = {
    enable = mkEnableOption "Enable support for pulseaudio.";
    support32Bit = mkEnableOption ''
      Whether to include the 32-bit pulseaudio libraries in the system or not.
    '';
  };

  config = mkIf cfg.enable (mkMerge [
    {
      sound.enable = true;
      hardware = {
        pulseaudio = {
          enable = true;
          package = pkgs.pulseaudioFull;
        };
      };
    }
    (mkIf cfg.support32Bit {
      hardware.pulseaudio = {
        support32Bit = cfg.support32Bit;
      };
    })
  ]);
}
