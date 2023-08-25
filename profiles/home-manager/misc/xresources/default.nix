{
  config,
  lib,
  pkgs,
  ...
}: {
  xresources = {
    path = "${config.xdg.configHome}/Xresources.d/Xresources";
    extraConfig = ''
      ! -- cursor theme -- !
      Xcursor.theme                         : Breeze_Snow
      Xcursor.size                          : 24

      ! -- Base16 OneDark -- !
      ! Scheme: Lalit Magant (http://github.com/tilal6991)

      #define base00 #282c34
      #define base01 #353b45
      #define base02 #3e4451
      #define base03 #545862
      #define base04 #565c64
      #define base05 #abb2bf
      #define base06 #b6bdca
      #define base07 #c8ccd4
      #define base08 #e06c75
      #define base09 #d19a66
      #define base0A #e5c07b
      #define base0B #98c379
      #define base0C #56b6c2
      #define base0D #61afef
      #define base0E #c678dd
      #define base0F #be5046

      *foreground                           : base05
      #ifdef background_opacity
      *background                           : [background_opacity]base00
      #else
      *background                           : base00
      #endif
      *cursorColor                          : base05

      *color0                               : base00
      *color1                               : base08
      *color2                               : base0B
      *color3                               : base0A
      *color4                               : base0D
      *color5                               : base0E
      *color6                               : base0C
      *color7                               : base05

      *color8                               : base03
      *color9                               : base08
      *color10                              : base0B
      *color11                              : base0A
      *color12                              : base0D
      *color13                              : base0E
      *color14                              : base0C
      *color15                              : base07

      ! Note: colors beyond 15 might not be loaded (e.g., xterm, urxvt),
      ! use 'shell' template to set these if necessary
      *color16                              : base09
      *color17                              : base0F
      *color18                              : base01
      *color19                              : base02
      *color20                              : base04
      *color21                              : base06

      ! -- 字体 -- !
      ! -- xft -- !
      ! hintstyle : hintnone | hintslight | hintmedium | hintfull
      ! lcdfilter : lcdnone  | lcddefault | lcdlight   | lcdlegacy
      Xft.dpi                               : 106
      Xft.antialias                         : true
      Xft.autohint                          : false
      Xft.rgba                              : rgb
      Xft.lcdfilter                         : lcddefault
      Xft.hinting                           : true
      Xft.hintstyle                         : hintslight
    '';
  };
}
