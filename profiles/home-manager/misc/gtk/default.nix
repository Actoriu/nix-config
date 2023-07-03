{
  config,
  lib,
  pkgs,
  ...
}: {
  gtk = {
    enable = true;
    gtk2 = {
      configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
    };
    # cursorTheme = {
    #   name = "Adwaita";
    #   size = "24";
    # };
    # font = {
    #   name = "Sans";
    #   size = "13";
    # };
    iconTheme = {
      package = pkgs.breeze-icons;
      name = "breeze-dark";
    };
    theme = {
      package = pkgs.breeze-gtk;
      name = "Breeze-Dark";
    };
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = true;
    };
    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = true;
    };
  };

  # GTK4
  dconf = {
    enable = true;
    settings = {
      "org/gnome/desktop/interface" = {
        color-scheme = "prefer-dark";
        cursor-theme = "Breeze_Snow";
        enable-animations = false;
        gtk-theme = "Breeze-Dark";
        icon-theme = "breeze-dark";
      };
      "org/gnome/desktop/wm/preferences" = {
        theme = "Breeze-Dark";
      };
    };
  };

  home.pointerCursor = {
    package = pkgs.libsForQt5.breeze-qt5;
    name = "Breeze_Snow";
    size = 24;
    gtk.enable = true;
    x11 = {
      enable = true;
      defaultCursor = "Breeze_Snow";
    };
  };
}
