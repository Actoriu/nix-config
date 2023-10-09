{
  config,
  lib,
  pkgs,
  ...
}: {
  programs = {
    urxvt = {
      enable = true;
      package = pkgs.rxvt-unicode.override {
        configure = {availablePlugins, ...}: {
          extraDeps = with pkgs; [xclip];
          plugins = with availablePlugins; [perls resize-font vtwheel];
        };
      };
      extraConfig = {
        font = "xft:Sarasa Fixed SC:style=Regular:pixelsize=24,xft:Noto Color Emoji:style=Regular:pixelsize=24:minspace=false,xft:Symbols Nerd Font Mono:style=Regular:pixelsize=24:minspace=false";
        boldFont = "xft:Sarasa Fixed SC:style=Bold:pixelsize=24";
        italicFont = "xft:Sarasa Fixed SC:style=Italic:pixelsize=24";
        boldItalicFont = "xft:Sarasa Fixed SC:style=Bold Italic:pixelsize=24";
        depth = 32;
        reverseVideo = false;
        loginShell = true;
        fading = 30;
        visualBell = true;
        letterSpace = -1;
        print-pipe = ''"cat > /dev/null"'';
        preeditType = "Root";
        skipBuiltinGlyphs = true;
        inheritPixmap = true;
        transparent = false;
        shading = 15;
        termName = "xterm-256color";
        title = "URxvt";
        geometry = "80x20+280+120";
        cutchars = ''"\\ `\"\'&()*,;<_=>?%#@~[-A-Za-z0-9]{|}.:+-"'';
        iso14755 = false;
        iso14755_52 = false;
        scrollBar = false;
        scrollBar_right = true;
        scrollBar_floating = false;
        scrollstyle = "rxvt";
        scrollTtyOutput = false;
        scrollTtyKeypress = true;
        scrollWithBuffer = true;
        skipScroll = true;
        mouseWheelScrollPage = true;
        saveLines = 65536;
        secondaryScreen = true;
        secondaryScroll = true;
        secondaryWheel = true;
        cursorBlink = true;
        cursorUnderline = false;
        internalBorder = 0;
        externalBorder = 0;
        borderLess = false;
        perl-ext-common = "default,matcher,clipboard,url-select,keyboard-select";
        "clipboard.copycmd" = "xclip -i -selection clipboard";
        "clipboard.pastecmd" = "xclip -o -selection clipboard";
        "clipboard.autocopy" = true;
        "keysym.S-C-C" = "perl:clipboard:copy";
        "keysym.S-C-V" = "perl:clipboard:paste";
        "keysym.C-Escape" = "perl:clipboard:paste_escaped";
        "keysym.M-Escape" = "perl:keyboard-select:activate";
        "keysym.M-s" = "perl:keyboard-select:search";
        "keysym.M-u" = "perl:url-select:select_next";
        "url-select.autocopy" = true;
        "url-select.button" = 1;
        "url-select.launcher" = "/usr/bin/xdg-open";
        "url-select.underline" = true;
      };
    };
  };

  /*
  home.packages = with pkgs; [
    (rxvt-unicode.override {
      configure = {availablePlugins, ...}: {
        extraDeps = [xclip];
        plugins = with availablePlugins; [perls resize-font vtwheel];
      };
    })
  ];
  */
}
