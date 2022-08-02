{ self
, inputs
, ...
}: {
  actoriu = inputs.home-manager.lib.homeManagerConfiguration {
    modules = [
      ({ config, lib, pkgs, ... }: {
        nixpkgs = {
          config = { allowUnfree = true; };
          verlays = [
            (final: prev: { spacemacs = inputs.spacemacs; })
          ];
        };
      })
      ../../user/actoriu
    ];
  };
}
