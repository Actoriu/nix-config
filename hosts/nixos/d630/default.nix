{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./configuration.nix
    ../../../modules/nixos/loader/default.nix
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
