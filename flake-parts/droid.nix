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
      extraModules ? [],
      hostname ? "default",
      username ? null,
      system ? "aarch64-linux",
      stateVersion ? lib.fileContents ../.version,
      ...
    }: {
      ${hostname} = inputs.nix-on-droid.lib.nixOnDroidConfiguration {
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
          ++ lib.optional (hostname != null) ../hosts/${hostname}
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

              system.stateVersion = stateVersion;

              home-manager = {
                extraSpecialArgs = {inherit username stateVersion;};
                # useGlobalPkgs = true;
                useUserPackages = true;
                config = {
                  config,
                  lib,
                  pkgs,
                  ...
                }: {
                  imports =
                    [
                      inputs.impermanence.nixosModules.home-manager.impermanence
                      inputs.nur.hmModules.nur
                      inputs.sops-nix.homeManagerModules.sops
                    ]
                    ++ lib.optional (username != null) ../users/${username};

                  nixpkgs = {
                    config = {
                      allowUnfree = true;
                      # Workaround for https://github.com/nix-community/home-manager/issues/2942
                      allowUnfreePredicate = _: true;
                    };
                    # overlays = builtins.attrValues outputs.overlays;
                  };

                  home.stateVersion = stateVersion;
                  programs.home-manager.enable = true;
                  manual.manpages.enable = false;
                };
              };
            })
          ];
      };
    };
  in {
    legacyPackages.droidConfigurations = lib.mkMerge [
      (mkDroidConfig {
        hostname = "oneplus5";
        username = "nix-on-droid";
        system = "aarch64-linux";
        # extraModules = [];
      })
    ];
  };
}
