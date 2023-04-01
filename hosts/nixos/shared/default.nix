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
  imports =
    [
      inputs.impermanence.nixosModules.impermanence
      inputs.nixos-cn.nixosModules.nixos-cn-registries
      inputs.nixos-cn.nixosModules.nixos-cn
      inputs.nur.nixosModules.nur
      inputs.sops-nix.nixosModules.sops
      inputs.home-manager.nixosModules.home-manager
      ../../../profiles/nixos
      (../. + "/${hostname}")
    ]
    ++ (builtins.attrValues outputs.nixosModules);

  networking.hostName = hostname;

  # system.stateVersion = "${version}";

  nixpkgs = {
    config = {
      allowUnfree = true;
      # for nvidia
      allowBroken = true;
    };
    hostPlatform = lib.mkDefault system;
    overlays = builtins.attrValues outputs.overlays;
    # overlays = [
    #   # Add overlays your own flake exports (from overlays and pkgs dir):
    #   outputs.overlays.additions
    #   outputs.overlays.modifications
    #   outputs.overlays.spacemacs

    #   # You can also add overlays exported from other flakes:
    #   inputs.nixos-cn.overlay
    #   inputs.nur.overlay
    #   inputs.sops-nix.overlays.default
    # ];
  };

  home-manager = {
    # useGlobalPkgs = true;
    useUserPackages = true;
    extraSpecialArgs = {inherit inputs outputs version;};
    users.${username} = {...}: {
      # home.stateVersion = "${version}";
      # programs.home-manager.enable = true;
      # manual.manpages.enable = false;
      # systemd.user.startServices = "sd-switch";
      imports = [
        # inputs.impermanence.nixosModules.home-manager.impermanence
        # inputs.nur.hmModules.nur
        # inputs.sops-nix.homeManagerModules.sops
        # ../../../modules/home-manager
        (../../../users/. + "/${username}")
      ];
    };
  };
}
