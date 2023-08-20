{
  config,
  lib,
  pkgs,
  ...
}: {
  fonts = {
    packages = with pkgs; [
      (nerdfonts.override {fonts = ["Iosevka" "NerdFontsSymbolsOnly"];})
      noto-fonts
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
}
