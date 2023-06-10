{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./configuration.nix
  ];

  custom = {
    i18n = {
      enable = true;
      biostype = "legacy";
      bootloader = "grub";
      device = "/dev/sda";
      disktype = "mbr";
    };
  };
}
