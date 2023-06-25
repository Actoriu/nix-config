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
      inputs.home-manager.nixosModules.home-manager
    ]
    ++ lib.optional (hostname != null) ../profiles/hosts/${hostname};

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
