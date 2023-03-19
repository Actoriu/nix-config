{
  config,
  desktop,
  hostname,
  inputs,
  lib,
  outputs,
  pkgs,
  system,
  username,
  version,
  ...
}: {
  imports = [
    inputs.impermanence.nixosModules.impermanence
    inputs.nixos-cn.nixosModules.nixos-cn-registries
    inputs.nixos-cn.nixosModules.nixos-cn
    inputs.nur.nixosModules.nur
    inputs.sops-nix.nixosModules.sops
    inputs.home-manager.nixosModules.home-manager
    ../../../modules/nixos
    ../../../profiles/nixos
    (../. + "/${hostname}")
  ];

  networking.hostName = hostname;

  system.stateVersion = "${version}";

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = true;
      allowUnsupportedSystem = true;
    };
    hostPlatform = lib.mkDefault system;
    overlays = [
      inputs.nixos-cn.overlay
      inputs.nur.overlay
      inputs.sops-nix.overlays.default
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.spacemacs
    ];
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {inherit inputs outputs version;};
    users.${username} = {...}: {
      home.stateVersion = "${version}";
      programs.home-manager.enable = true;
      manual.manpages.enable = false;
      systemd.user.startServices = "sd-switch";
      imports = [
        inputs.impermanence.nixosModules.home-manager.impermanence
        ../../../modules/home-manager
        ../../../users/${username}
      ];
    };
  };
}
