{
  inputs,
  lib,
  ...
}: let
  mkHomeConfig = {
    extraModules ? [],
    hostname ? null,
    non-nixos ? true,
    system ? "x86_64-linux",
    stateVersion ? lib.fileContents ../.version,
    username ? null,
    ...
  }: {
    "${username}@${hostname}" = inputs.home-manager.lib.homeManagerConfiguration {
      pkgs = inputs.nixpkgs.legacyPackages."x86_64-linux";
      extraSpecialArgs = {inherit inputs non-nixos username stateVersion;};
      modules =
        extraModules
        ++ [./home.nix];
    };
  };
in {
  flake.homeConfigurations = lib.mkMerge [
    (mkHomeConfig {
      hostname = "d630";
      username = "actoriu";
      extraModules = [
        inputs.impermanence.nixosModules.home-manager.impermanence
        inputs.nix-doom-emacs.hmModule
        # inputs.nur.hmModules.nur
        inputs.sops-nix.homeManagerModules.sops
      ];
    })
  ];
}
