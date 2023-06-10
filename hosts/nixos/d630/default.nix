{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./configuration.nix
  ];

  # customize = {
  #   loader = {
  #     enable = true;
  #     biostype = "legacy";
  #     bootloader = "grub";
  #     device = "/dev/sda";
  #     disktype = "mbr";
  #   };
  # };
}
