{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./configuration.nix
    ../../../modules/nixos/module-list.nix
    ../../nixos/fonts
    ../../nixos/hardware/audio/pipewire
    ../../nixos/hardware/bluetooth
    ../../nixos/hardware/cpu/intel
    ../../nixos/hardware/opengl
    ../../nixos/hardware/printers
    ../../nixos/hardware/video/nvidia/340xx
    ../../nixos/networking/networkmanager
    ../../nixos/nix
    ../../nixos/power-management/acpid
    ../../nixos/power-management/powertop
    ../../nixos/power-management/tlp
    ../../nixos/power-management/upower
    ../../nixos/security/polkit
    ../../nixos/services/laptop
    ../../nixos/shell/zsh
  ];

  custom = {
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
