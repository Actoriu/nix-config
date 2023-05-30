{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.custom.xresources;
in {
  options.custom.xresources = {
    enable = mkEnableOption "Enable support for xresources.";
  };

  config = mkIf cfg.enable {
    xresources = {
      extraConfig = ''
        ! -- cursor theme -- !
        Xcursor.theme                         : Breeze
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

        ! -- St -- !

        ! These are all possible options, presented with default values.
        ! xst will fallback to *.option if st.option is not present.
        ! see src/config.h for more information about option meanings.

        ! --- These options only take effect on startup. ---
        st.termname                           : xterm-256color

        ! --- The following options options can be reloaded via USR1 signal. ---
        st.font                               : Sarasa Mono SC:pixelsize=24,Sarasa Mono SC Nerd:pixelsize=24,Noto Color Emoji,Symbols Nerd Font

        ! -- URxvt -- !

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

        ! px=pt*DPI/72
        ! fc-match Sans-16 family autohint hinting hintstyle dpi pixelsize antialias rgba lcdfilter
        ! Fallback fonts with wide symbols in them need the pixelsize set to a really
        ! small value since urxvt doesnt know what to do when it encounters a glyph
        ! that is wider than the main font.

        URxvt.font                            : xft:Sarasa Mono SC:pixelsize=24,xft:Sarasa Mono SC Nerd:pixelsize=24,xft:Noto Color Emoji:pixelsize=16,xft:Symbols Nerd Font:pixelsize=16

        URxvt.boldFont                        : xft:Sarasa Mono SC:bold:pixelsize=24,xft:Sarasa Mono SC Nerd:bold:pixelsize=24

        URxvt.italicFont                      : xft:Sarasa Mono SC:italic:pixelsize=24,xft:Sarasa Mono SC Nerd:italic:pixelsize=24

        URxvt.boldItalicFont                  : xft:Sarasa Mono SC:bold:italic:pixelsize=24,xft:Sarasa Mono SC Nerd:bold:italic:pixelsize=24

        ! -- 颜色 -- !
        URxvt.depth                           : 32

        ! -- Misc -- !
        URxvt.reverseVideo                    : false
        URxvt.loginShell                      : true
        URxvt.fading                          : 30
        URxvt.visualBell                      : true
        ! URxvt.lineSpace                       : 0
        URxvt.letterSpace                     : 0
        ! URxvt.iconFile                        : /usr/share/icons/Adwaita/24x24/apps/utilities-terminal.png
        URxvt.print-pipe                      : cat > /tmp/$(echo urxvt.dump.$(date +'%Y%M%d%H%m%S'))

        ! -- 渲染 -- !
        URxvt.preeditType                     : Root
        URxvt.skipBuiltinGlyphs               : true

        ! -- 伪透明 -- !
        URxvt.inheritPixmap                   : false
        URxvt.transparent                     : false
        ! -- 透明度 -- !
        URxvt.shading                         : 15

        ! -- 标题 -- !
        URxvt.termName                        : xterm-256color
        URxvt.title                           : URxvt

        ! -- 大小/位置 -- !
        URxvt.geometry                        : 80x20+280+120

        ! -- 光标字符选择 -- !
        ! URxvt.cutchare                        : "-A-Za-z0-9,./?%&#:_=+@~"
        URxvt.cutchars                        : "\\ `\"\'&()*,;<_=>?%#@~[-A-Za-z0-9]{|}.:+-"

        ! -- Ctrl+Shift的输入特殊字符功能 -- !
        URxvt.iso14755                        : false
        URxvt.iso14755_52                     : false

        ! -- 滚动条 -- !
        URxvt.scrollBar                       : false
        URxvt.scrollBar_right                 : true
        URxvt.scrollBar_floating              : false
        URxvt.scrollstyle                     : rxvt
        URxvt.scrollTtyOutput                 : false
        URxvt.scrollTtyKeypress               : true
        URxvt.scrollWithBuffer                : false
        URxvt.skipScroll                      : true

        ! -- 滚屏 -- !
        URxvt.mouseWheelScrollPage            : true
        URxvt.saveLines                       : 65536
        URxvt.secondaryScreen                 : true
        URxvt.secondaryScroll                 : true
        URxvt.secondaryWheel                  : true

        ! -- 光标 -- !
        URxvt.cursorBlink                     : true
        URxvt.cursorUnderline                 : false

        ! -- 边框 -- !
        URxvt.internalBorder                  : 0
        URxvt.externalBorder                  : 0
        URxvt.borderLess                      : false

        ! -- 扩展 -- !
        ! URxvt.perl-lib                        : /usr/lib/urxvt/perl/
        URxvt.perl-ext-common                 : default,matcher,clipboard,url-select,keyboard-select
        ! -- clipboard -- !
        URxvt.clipboard.copycmd               : xclip -i -selection clipboard
        URxvt.clipboard.pastecmd              : xclip -o -selection clipboard
        URxvt.clipboard.autocopy              : true
        URxvt.keysym.S-C-C                    : perl:clipboard:copy
        URxvt.keysym.S-C-V                    : perl:clipboard:paste
        URxvt.keysym.C-Escape                 : perl:clipboard:paste_escaped
        ! -- keyboard-select -- !
        URxvt.keysym.M-Escape                 : perl:keyboard-select:activate
        URxvt.keysym.M-s                      : perl:keyboard-select:search

        ! -- 超链接 -- !
        URxvt.keysym.M-u                      : perl:url-select:select_next
        URxvt.url-select.autocopy             : true
        URxvt.url-select.button               : 1
        ! URxvt.url-select.launcher             : /usr/bin/xdg-open
        URxvt.url-select.underline            : true

        ! -- XTerm -- !

        ! Application Resources
        XTerm*utf8                            : 1
        XTerm*VT100.locale                    : 1
        XTerm*termName                        : xterm-256color
        XTerm*utmpInhibit                     : 1

        ! VT100 Widget Resources
        XTerm*allowTitleOps                   : 0
        XTerm*altSendsEscape                  : 1
        XTerm*visualbell                      : 1
        XTerm*bellIsUrgent                    : 1
        XTerm*borderLess                      : 1
        ! allow selecting email/url by double click
        XTerm*selectToClipboard               : 1
        XTerm*charClass                       : 33:48,35:48,37-38:48,43:48,45-47:48,58:48,61:48,63:48,64:48,126:48
        ! XTerm*on3Clicks                       : regex ([[:alpha:]]+://)?([[:alnum:]!#+,./=?@_~-]|(%[[:xdigit:]][[:xdigit:]]))+
        XTerm*dynamicColors                   : 1
        XTerm*colorBDMode                     : 1
        XTerm*colorULMode                     : 1
        XTerm*cursorBlink                     : 1
        XTerm*cursorUnderLine                 : 1
        XTerm*eightBitInput                   : 0
        ! uncomment to output a summary of each font s metrics
        ! xterm.reportFonts                    : 1
        XTerm*fontMenu*fontdefault*Label      : Default
        XTerm*faceName                        : Iosevka:antialias=1:pixelsize=24
        ! xterm*faceNameDoublesize              : WenQuanYi Micro Hei Mono:antialias=1:pixelsize=23
        XTerm*cjkWidth                        : 1
        XTerm*faceNameDoublesize              : Noto Sans Mono CJK SC:antialias=1:pixelsize=24
        XTerm*faceSize                        : 12.5
        XTerm*faceSize1                       : 14
        XTerm*faceSize2                       : 15
        XTerm*faceSize3                       : 16
        XTerm*faceSize4                       : 18
        XTerm*faceSize5                       : 20
        XTerm*faceSize6                       : 22
        XTerm*fastScroll                      : 1
        XTerm*highlightSelection              : 1
        XTerm*jumpScroll                      : 1
        XTerm*loginshell                      : 1
        XTerm*multiScroll                     : 1
        XTerm*printAttributes                 : 2
        XTerm*printerCommand                  : xterm -T History -e sh -c 'less -r -o /tmp/xterm.dump <&3' 3<&0
        XTerm*rightScrollBar                  : 1
        XTerm*saveLines                       : 65535
        XTerm*scrollBar                       : 0
        XTerm*trimSelection                   : 1
        XTerm*veryBoldColors                  : 4
        XTerm*xftAntialias                    : 1

        XTerm*VT100.Translations              : #override
        Ctrl Shift <Key>C                     : copy-selection(CLIPBOARD) \n
        Ctrl Shift <Key>V                     : insert-selection(CLIPBOARD) \n
        Ctrl <Key>slash                       : print-everything() \n
        Ctrl <Btn1Up>                         : exec-formatted("xdg-open '%t'", PRIMARY)
      '';
    };
  };
}
