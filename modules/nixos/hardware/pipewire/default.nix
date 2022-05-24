{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.custom.hardware.pipewire;
in {
  options.custom.hardware.pipewire = {
    enable = mkEnableOption "Enable support for pipewire.";
    support32Bit = mkEnableOption ''
      Whether to enable 32-bit ALSA support on 64-bit systems.
    '';
  };

  config = mkIf cfg.enable (mkMerge [
    {
      sound.enable = false;
      security.rtkit.enable = true;
      services.pipewire = {
        enable = true;
        alsa.enable = true;
        pulse.enable = true;
        jack.enable = true;
      };
    }
    (mkIf cfg.support32Bit {
      services.pipewire = {
        alsa.support32Bit = cfg.support32Bit;
      };
    })
  ]);
}
