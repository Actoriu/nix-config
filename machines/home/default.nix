{ self
, inputs
, ...
}:

let
  system = "x86_64-linux";
  pkgs = inputs.nixos.legacyPackages.${system};
in
{
  homeConfigurations = {
    actoriu = inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;

      modules = [
        ({ config, lib, pkgs, ... }: {
          nixpkgs = {
            config = { allowUnfree = true; };
            verlays = [
              self.verlays.default
              (final: prev: { spacemacs = inputs.spacemacs; })
            ];
          };
        })
        ../../user/actoriu
      ];
    };
  };
};
