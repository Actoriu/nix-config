{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.custom.fonts;
in {
  options.custom.fonts = {
    enable = mkEnableOption "Enable support for fonts.";
  };

  config = mkIf cfg.enable {
    fonts = {
      fonts = with pkgs; [
        (nerdfonts.override {fonts = ["Iosevka"];})
        noto-fonts
        noto-fonts-extra
        noto-fonts-emoji
      ];
      fontDir.enable = true;
      enableGhostscriptFonts = true;
      fontconfig = {
        defaultFonts = {
          emoji = ["Noto Color Emoji"];
        };
      };
    };
  };
}
