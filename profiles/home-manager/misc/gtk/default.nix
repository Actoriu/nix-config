{
  config,
  lib,
  pkgs,
  ...
}: {
  gtk = {
    enable = true;
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

  home.pointerCursor = {
    package = pkgs.libsForQt5.breeze-qt5;
    name = "breeze_cursors";
    size = 24;
    gtk.enable = true;
    x11 = {
      enable = true;
      defaultCursor = "breeze_cursors";
    };
  };
}
