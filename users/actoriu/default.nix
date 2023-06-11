{
  config,
  lib,
  non-nixos,
  pkgs,
  ...
}: {
  imports = [
    ../../modules/home-manager/editors/emacs
    ../../profiles/home-manager/i18n/input-method/fcitx5
    ../../profiles/home-manager/misc/fontconfig
    ../../profiles/home-manager/misc/xdg
    ../../profiles/home-manager/misc/xresources
  ];
}
