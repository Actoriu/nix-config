{
  inputs,
  lib,
  stateVersion,
  self,
  ...
}: {
  perSystem = {
    config,
    lib,
    pkgs,
    ...
  }: let
    buildSuites = profiles: f: lib.mapAttrs (_: lib.flatten) (lib.fix (f profiles));

    defaultModules = [
      # make flake inputs accessible in NixOS
      {
        _module.args.self = self;
        _module.args.inputs = inputs;
        _module.args.lib = lib;
      }
    ];

    mkDroidConfig = {
      devicename ? "default",
      extraModules ? [],
      hostname ? null,
      username ? null,
      system ? "aarch64-linux",
      ...
    }: {
      ${devicename} = inputs.nix-on-droid.lib.nixOnDroidConfiguration {
        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            inputs.nix-on-droid.overlays.default
          ];
        };
        extraSpecialArgs = {inherit username stateVersion;};
        home-manager-path = inputs.home-manager.outPath;
        modules =
          defaultModules
          ++ extraModules
          ++ lib.optional (hostname != null) ../hosts/droid/${hostname}
          ++ [
            ({
              config,
              lib,
              ...
            }: {
              nixpkgs = {
                config = {
                  allowUnfree = true;
                  # for build nvidia
                  allowBroken = true;
                };
              };

              system.stateVersion = "${stateVersion}";

              home-manager = {
                extraSpecialArgs = {inherit username stateVersion;};
                # useGlobalPkgs = true;
                useUserPackages = true;
                config = ./home.nix ;
              };
            })
          ];
      };
    };
  in {
    legacyPackages.droidConfigurations = lib.mkMerge [
      (mkDroidConfig {
        # devicename = "oneplus5";
        extraModules = [];
      })
    ];
  };
}
