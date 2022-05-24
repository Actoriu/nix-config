{ config, lib, pkgs, ... }:

with lib;
with builtins;

let
  cfg =  config.custom;
in

rec {
  imports = [
    ./scripts.nix
    ./nixos.nix
    ./software.nix
    ./security.nix
    ./regional.nix
    ./network.nix
    ./user.nix
  ];

  options.custom = {
    kernelPackage = mkOption {
      default = pkgs.linuxPackages_latest;
      description = "Kernel package used to build this system";
    };

    diskLayout = mkOption {
      type = types.enum [ "btrfs" "btrfs-crypt" "vm" ];
      description = "This is the layout of the disk used by the system.";
      default = "btrfs";
    };

    bootloader = mkOption {
      type = types.enum [ "grub" "systemd-boot" ];
      description = "The boot loader used to boot the system";
      default = "systemd-boot";
    };

    biosType = mkOption {
      type = types.enum [ "uefi" "legacy" ];
      description = "Specify the bios type of the machine";
    };

    bootLogLevel = mkOption {
      type = types.ints.unsigned;
      description = "Log level of the kernel console output. Must be 0 to 7. 3 being errors only";
      default = 3;
    };

    bluetooth = mkEnableOption "System has a bluetooth adapter";

    cpu = {
      type = mkOption {
        type = types.enum [ "intel_legacy" "intel" "amd" "amd_legacy" ];
        description = "Type of cpu the system has in it";
      };

      cores = mkOption {
        type = types.int;
        default = 1;
        description = "Number of physical cores on cpu per socket";
      };

      sockets = mkOption {
        type = types.int;
        default = 1;
        description = "Number of CPU sockets installed in system";
      };

      threadsPerCore = mkOption {
        type = types.int;
        default = 1;
        description = "Number of threads per core.";
      };

      sensorCommand = mkOption {
        type = types.str;
        description = "Command to get cpu temp";
      };
    };
  };

  config = {

    # Earlyoom prevents systems from locking up when they run out of memory
    services.earlyoom.enable = true;
    services.fstrim.enable = true;

    # TTY font
    # console.font = lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";

    # Enable all unfree hardware support.
    hardware.firmware = with pkgs; [ firmwareLinuxNonfree ];
    hardware.enableAllFirmware = true;
    hardware.enableRedistributableFirmware = true;
    nixpkgs.config.allowUnfree = true;

    boot.kernelPackages = cfg.kernelPackage;
    boot.kernelParams = [
      (mkIf (cfg.cpu.type == "intel") "intel_pstate=active")
    ];

    boot.consoleLogLevel = cfg.bootLogLevel;

    environment.systemPackages = with pkgs; [
      (mkIf (cfg.cpu.type == "amd") microcodeAmd)
      (mkIf (cfg.cpu.type == "intel") microcodeIntel)
    ];

    boot.loader.systemd-boot.enable = cfg.bootloader == "systemd-boot";
    boot.loader.efi.canTouchEfiVariables = cfg.bootloader == "systemd-boot";

    nix.settings.max-jobs = cfg.cpu.cores * cfg.cpu.threadsPerCore * cfg.cpu.sockets;

    hardware.bluetooth.enable = cfg.bluetooth;
    # services.blueman.enable = cfg.bluetooth;

    # This is the main layout I have on my systems.
    # It works by using the correct labels for drives.
    boot.initrd.luks.devices."cryptroot".device = (mkIf (cfg.diskLayout == "btrfs-crypt") "/dev/disk/by-label/CRYPTROOT");

    fileSystems."/" = (if (cfg.diskLayout == "btrfs-crypt") then
      { device = "/dev/disk/by-label/ROOT";
        fsType = "btrfs";
        options = [ "subvol=@" "discard=async" ];
      }
                       else
                         {
                           device = "/dev/disk/by-label/ROOT";
                           fsType = "auto";
                           options = [ ];
                         });

    fileSystems."/home" = (mkIf (cfg.diskLayout == "btrfs-crypt")
      { device = "/dev/disk/by-label/ROOT";
        fsType = "btrfs";
        options = [ "subvol=@home" "discard=async"  ];
      });

    fileSystems."/var" = (mkIf (cfg.diskLayout == "btrfs-crypt")
      { device = "/dev/disk/by-label/ROOT";
        fsType = "btrfs";
        options = [ "subvol=@var" "discard=async" ];
      });

    fileSystems."/.pagefile" = (mkIf (cfg.diskLayout == "btrfs-crypt")
      { device = "/dev/disk/by-label/ROOT";
        fsType = "btrfs";
        options = [ "subvol=@pagefile" "discard=async" ];
      });

    fileSystems."/boot" = (mkIf (cfg.diskLayout == "btrfs-crypt")
      { device = "/dev/disk/by-label/BOOT";
        fsType = "vfat";
      });

    swapDevices = (mkIf (cfg.diskLayout == "btrfs-crypt")[
      {
        device = "/.pagefile/pagefile";
      }
    ]);
  };
}
