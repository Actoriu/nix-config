{ self
, inputs
, pkgs
, ...
}: {
  actoriu = inputs.home-manager.lib.homeManagerConfiguration {
    inherit pkgs;

    modules = [
      ({ config, lib, pkgs, ... }: {
        nixpkgs = {
          config = { allowUnfree = true; };
          overlays = [
            self.overlays.default
            (final: prev: { spacemacs = inputs.spacemacs; })
          ];
        };
      })
      ../../users/actoriu
    ];
  };
}
