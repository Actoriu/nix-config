{ config, lib, ... }:

with lib;

let
  cfg = config.custom.system.filesystems;
in
{
  options.custom.system.filesystems = {
    enable = mkEnableOption "Enable support for filesystems.";
  };

  config = mkIf cfg.enable {
    fileSystems = {
      "/" = {
        device = "/dev/disk/by-label/root";
        fsType = "btrfs";
        options = [ "subvol=root" "compress=zstd" "noatime" ];
      };
      "/nix" = {
        device = "/dev/disk/by-label/nix";
        fsType = "btrfs";
        options = [ "subvol=nix" "compress=zstd" "noatime" ];
      };
      "/home" = {
        device = "/dev/disk/by-label/home";
        fsType = "btrfs";
        options = [ "subvol=home" "compress=zstd" "noatime" ];
      };
    };
    swapDevices = [{ device = "/swapfile"; size = 6*1024; }];
  };
}
