{
  config,
  lib,
  non-nixos,
  pkgs,
  ...
}: {
  imports = [
    ../../../../profiles/home-manager/i18n/input-method/fcitx5/default.nix
    ../../../../profiles/home-manager/misc/fontconfig/default.nix
    ../../../../profiles/home-manager/misc/xdg/default.nix
    ../../../../profiles/home-manager/misc/xresources/default.nix
    ../../../../profiles/home-manager/non-nixos/default.nix
  ];
}
