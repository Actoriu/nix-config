{ config
, lib
, pkgs
, ...
}:

with lib;

let
  cfg = config.custom.shell.rofi;
in
{
  options.custom.shell.rofi = {
    enable = mkEnableOption "Enable support for rofi.";
  };

  config = mkIf cfg.enable {
    programs = {
      rofi = {
        enable = cfg.enable;
        # package = with pkgs; [
        #   (rofi.override {
        #     plugins = with pkgs; [ rofi-calc rofi-emoji ];
        #   })
        # ];
        extraConfig = {
          modi = "window,run,ssh";
          width = 60;
          lines = 6;
          font = "monospace 16";
          show-icons = true;
          icon-theme = "Arc";
          disable-history = true;
          matching = "fuzzy";
          theme = "Arc-Dark";
        };
      };
    };
  };
}
