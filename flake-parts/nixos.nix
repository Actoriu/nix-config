{
  config,
  desktop,
  hostname,
  inputs,
  lib,
  non-nixos,
  outputs,
  pkgs,
  system,
  username,
  stateVersion,
  ...
}: {
  imports =
    [
      inputs.disko.nixosModules.disko
      inputs.home-manager.nixosModules.home-manager
      inputs.impermanence.nixosModules.impermanence
      inputs.nur.nixosModules.nur
      inputs.sops-nix.nixosModules.sops
      ../../../modules/nixos
      ../../../profiles/nixos
    ]
    ++ lib.optional (hostname != null) ../${hostname};

  networking.hostName = hostname;

  system.stateVersion = "${stateVersion}";

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
    extraSpecialArgs = {inherit desktop hostname inputs non-nixos outputs username stateVersion;};
    # useGlobalPkgs = true;
    useUserPackages = true;
    users.${username} = import ./home.nix;
  };
}
