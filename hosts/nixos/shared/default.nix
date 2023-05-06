{
  config,
  desktop,
  homeManagerModules,
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
    inputs.nur.nixosModules.nur
    inputs.sops-nix.nixosModules.sops
    inputs.home-manager.nixosModules.home-manager
    # ../../../modules/nixos
    ../../../profiles/nixos
    ../${hostname}
  ];

  networking.hostName = hostname;

  # system.stateVersion = "${version}";

  nixpkgs = {
    config = {
      allowUnfree = true;
      # for build nvidia
      allowBroken = true;
    };
    hostPlatform = lib.mkDefault system;
    overlays = builtins.attrValues outputs.overlays;
  };

  home-manager = {
    extraSpecialArgs = {inherit desktop hostname inputs outputs username version;};
    sharedModules = builtins.attrValues homeManagerModules;
    # useGlobalPkgs = true;
    useUserPackages = true;
    users.${username} = import ../../../users/shared;
  };
}
