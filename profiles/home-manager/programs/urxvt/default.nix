{
  config,
  lib,
  pkgs,
  ...
}: {
  # programs = {
  #   urxvt = {
  #     enable = true;
  #   };
  # };

  home.packages = with pkgs; [
    (rxvt-unicode.override {
      configure = {availablePlugins, ...}: {
        extraDeps = [xclip];
        plugins = with availablePlugins; [perls resize-font vtwheel];
      };
    })
  ];
}
