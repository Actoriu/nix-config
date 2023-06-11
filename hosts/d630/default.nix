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
    loader = {
      enable = true;
      biostype = "legacy";
      bootloader = "grub";
      device = "/dev/sda";
      disktype = "mbr";
    };
  };
}
