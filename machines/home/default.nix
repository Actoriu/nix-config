{ self
, inputs
, pkgs
, ...
}: {
  actoriu = inputs.home.lib.homeManagerConfiguration {
    inherit pkgs;

    modules = [
      ({ config
       , lib
       , pkgs
       , ...
       }: {
        nixpkgs = {
          config = { allowUnfree = true; };
          overlays = [
            # self.overlay
            (final: prev: { spacemacs = inputs.spacemacs; })
          ];
        };
      })
      ../../users/actoriu
    ];
  };
}
