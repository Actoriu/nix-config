{
  config,
  lib,
  pkgs,
  profiles,
  suites,
  ...
}: {
  imports =
    [
      ./configuration.nix
      # ../../modules/nixos/module-list.nix
      # ../../profiles/nixos/fonts
      # ../../profiles/nixos/hardware/audio/pipewire
      # ../../profiles/nixos/hardware/bluetooth
      # ../../profiles/nixos/hardware/cpu/intel
      # ../../profiles/nixos/hardware/opengl
      # ../../profiles/nixos/hardware/printers
      # ../../profiles/nixos/hardware/video/nvidia/340xx
      # ../../profiles/nixos/networking/networkmanager
      # ../../profiles/nixos/nix
      # ../../profiles/nixos/power-management/acpid
      # ../../profiles/nixos/power-management/powertop
      # ../../profiles/nixos/power-management/tlp
      # ../../profiles/nixos/power-management/upower
      # ../../profiles/nixos/security/polkit
      # ../../profiles/nixos/services/laptop
      # ../../profiles/nixos/shell/zsh
    ]
    ++ (with suites; [
      base
      hardware
      graphical
      network
      power
      security
      services
      shell
    ]);

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
