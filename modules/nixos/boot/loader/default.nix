{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.private.loader;
in {
  options.private.loader = {
    enable = mkEnableOption "Enable support for loader.";

    biostype = mkOption {
      type = types.nullOr (types.enum ["gpt" "legacy"]);
      default = null;
      example = "legacy";
      description = "Enable support for bios type.";
    };

    bootloader = mkOption {
      type = types.nullOr (types.enum ["grub" "systemd-boot"]);
      default = null;
      example = "grub";
      description = "Enable support for Bootloader.";
    };

    disktype = mkOption {
      type = types.nullOr (types.enum ["uefi" "mbr"]);
      default = null;
      example = "mbr";
      description = "Enable support for disk partition table type.";
    };

    device = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "/dev/sda";
      description = "Enable support for grub device.";
    };

    efiSysMountPoint = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "/boot/efi";
      description = "Enable support for efiSysMountPoint.";
    };
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf (cfg.biostype == "legacy" && cfg.disktype == "mbr" && cfg.bootloader == "grub") {
      boot.loader.grub = {
        enable = true;
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
