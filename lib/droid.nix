{ self
, home-manager
, nix-on-droid
, nixpkgs
, templates
, ...
}:

let
  inherit (nixpkgs) lib;
  hosts = (import ./hosts.nix).nix-droid.all;

  nixRegistry = {
    nix.registry = {
      nixpkgs.flake = nixpkgs;
      p.flake = nixpkgs;
      pkgs.flake = nixpkgs;
      templates.flake = templates;
    };
  };

  genConfiguration = hostname: { hostPlatform, ... }:
    nix-on-droid.lib.nixOnDroidConfiguration {
      system = hostPlatform;
      pkgs = import nixpkgs {
        inherit (self.pkgs.${hostPlatform}) config;
        overlays = self.pkgs.${hostPlatform}.overlays ++ [ nix-on-droid.overlay ];
      };
      extraModules = [
        nixRegistry
      ];
      config = { ... }: {
        imports = [
          (../hosts + "/${hostname}")
        ];
      };
    };
in
lib.mapAttrs genConfiguration hosts
