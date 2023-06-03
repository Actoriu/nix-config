{
  config,
  lib,
  pkgs,
  ...
}: {
  programs = {
    rofi = {
      enable = true;
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
}
