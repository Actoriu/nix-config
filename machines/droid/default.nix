{ self
, inputs
, ...
}: {
  oneplus5 = inputs.nix-on-droid.lib.nixOnDroidConfiguration {
    system = "aarch64-linux";
    config = { config, lib, pkgs, ... }: {
      nixpkgs = {
        config = { allowUnfree = true; };
        verlays = [
          (final: prev: { spacemacs = inputs.spacemacs; })
        ];
      };

      imports = [
        ../../profiles/shared/home-manager
        ../../hosts/oneplus5
      ];

      home-manager = {
        config = { config, lib, pkgs, ... }: {
          nixpkgs = {
            config = { allowUnfree = true; };
            # verlays = [
            #   (final: prev: { spacemacs = inputs.spacemacs; })
            # ];
          };
          imports = [ ../../users/nix-on-droid/default.nix ];
        };
      };
    };
  };
}
