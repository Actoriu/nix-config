{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.custom.loader;
in {
  options.custom.loader = {
    enable = mkEnableOption "Enable support for loader.";

    biostype = mkOption {
      type = types.enum [ "gpt" "legacy" ];
      default = "legacy";
      description = "Enable support for bios type.";
    };

    bootloader = mkOption {
      type = types.enum [ "grub" "systemd-boot" ];
      default = "grub";
      description = "Enable support for Bootloader.";
    };

    disktype = mkOption {
      type = types.enum [ "uefi" "mbr" ];
      default = "mbr";
      description = "Enable support for disk partition table type.";
    };

    device = mkOption {
      type = types.str;
      default = "/dev/sda";
      description = "Enable support for grub device.";
    };

    efiSysMountPoint = mkOption {
      type = types.str;
      default = "/boot/efi";
      description = "Enable support for efiSysMountPoint.";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf (cfg.biostype == "legacy" && cfg.disktype == "mbr" && cfg.bootloader == "grub") {
      boot.loader.grub = {
        enable = true;
        version = 2;
        device = cfg.device;
        memtest86.enable = true;
        useOSProber = true;
      };
    })
    (mkIf (cfg.disktype == "uefi" && cfg.bootloader == "grub") {
      boot.loader = {
        efi = {
          canTouchEfiVariables = true;
          efiSysMountPoint = cfg.efiSysMountPoint;
        };
        grub = {
          enable = true;
          version = 2;
          device = cfg.device;
          efiSupport = true;
          efiInstallAsRemovable = true;
          memtest86.enable = true;
          useOSProber = true;
        };
      };
    })
    (mkIf (cfg.disktype == "uefi" && cfg.bootloader == "systemd-boot") {
      boot.loader = {
        efi = {
          canTouchEfiVariables = true;
          efiSysMountPoint = cfg.efiSysMountPoint;
        };
        systemd-boot = {
          enable = true;
          editor = false;
          memtest86.enable = true;
        };
      };
    })
  ]);
}
