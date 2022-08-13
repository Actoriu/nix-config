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
            self.verlays.default
            (final: prev: { spacemacs = inputs.spacemacs; })
          ];
        };
      })
      ../../user/actoriu
    ];
  };
}
