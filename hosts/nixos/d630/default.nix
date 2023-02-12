{
  inputs,
  outputs,
  ...
}: {
  imports = [
    inputs.impermanence.nixosModules.impermanence
    inputs.nixos-cn.nixosModules.nixos-cn-registries
    inputs.nixos-cn.nixosModules.nixos-cn
    inputs.sops-nix.nixosModules.sops
    inputs.home-manager.nixosModules.home-manager
    ./configuration.nix
    ../../../modules/nixos
    ../../../profiles/nixos
  ];

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = true;
      allowUnsupportedSystem = true;
    };
    overlays = builtins.attrValues outputs.overlays;
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {inherit inputs outputs;};
    users.actoriu = {...}: {
      home.stateVersion = "22.11";
      programs.home-manager.enable = true;
      manual.manpages.enable = false;
      systemd.user.startServices = "sd-switch";
      imports = [
        inputs.impermanence.nixosModules.home-manager.impermanence
        ../../../modules/home-manager
        ../../../users/actoriu
      ];
    };
  };

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
