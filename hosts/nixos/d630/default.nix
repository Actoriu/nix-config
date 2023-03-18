{
  inputs,
  outputs,
  pkgs,
  ...
}: {
  imports = [
    # inputs.impermanence.nixosModules.impermanence
    # inputs.nixos-cn.nixosModules.nixos-cn-registries
    # inputs.nixos-cn.nixosModules.nixos-cn
    # inputs.sops-nix.nixosModules.sops
    # inputs.home-manager.nixosModules.home-manager
    ./configuration.nix
    # ../../../modules/nixos
    # ../../../profiles/nixos
  ];

  custom = {
    fonts.enable = true;
    audio = {
      enable = true;
      pipewire = true;
    };
    bluetooth.enable = true;
    cpu.intel.enable = true;
    opengl.enable = true;
    printers.enable = true;
    video.nvidia = {
      enable = true;
      drivers = "nvidia-340";
    };
    loader = {
      enable = true;
      biostype = "legacy";
      bootloader = "grub";
      device = "/dev/sda";
      disktype = "mbr";
    };
    locale = {
      enable = true;
      inputMethod = "fcitx5";
      locale = "zh_CN";
    };
    network = {
      enable = true;
      networkmanager = true;
    };
    powerManagement = {
      enable = true;
      acpid = true;
      powertop = true;
      tlp = true;
      upower = true;
    };
    users = {
      enable = true;
      package = pkgs.zsh;
      userVersion = "22.11";
    };
  };
}
