{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./configuration.nix
    ../../modules/nixos/module-list.nix
  ];

  customize = {
    i18n = {
      enable = true;
      inputMethod = "fcitx5";
      locale = "zh_CN";
    };
    loader = {
      enable = true;
      biostype = "legacy";
      bootloader = "grub";
      device = "/dev/sda";
      disktype = "mbr";
    };
  };
}
