{ config
, lib
, pkgs
, ...
}:

with lib;

let
  cfg = config.custom.hardware.audio;
in
{
  options.custom.hardware.audio = {
    enable = mkEnableOption "Enable support for audio.";
    pipewire = mkEnableOption "Enable support for pipewire.";
    pipewire_support32Bit = mkEnableOption ''
      Whether to include the 32-bit alsa libraries in the system or not.
    '';
    pulseaudio = mkEnableOption "Enable support for pulseaudio.";
    pulseaudio_support32Bit = mkEnableOption ''
      Whether to include the 32-bit pulseaudio libraries in the system or not.
    '';
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf cfg.pipewire {
      sound.enable = false;
      security.rtkit.enable = true;
      services.pipewire = {
        enable = true;
        alsa.enable = true;
        pulse.enable = true;
        jack.enable = true;
      };
    })
    (mkIf (cfg.pipewire && cfg.pipewire_support32Bit) {
      services.pipewire = {
        alsa.support32Bit = cfg.pipewire_support32Bit;
      };
    })
    (mkIf cfg.pulseaudio {
      sound.enable = true;
      hardware = {
        pulseaudio = {
          enable = true;
          package = pkgs.pulseaudioFull;
        };
      };
    })
    (mkIf (cfg.pulseaudio && cfg.pulseaudio_support32Bit) {
      hardware.pulseaudio = {
        support32Bit = cfg.pulseaudio_support32Bit;
      };
    })
  ]);
}
