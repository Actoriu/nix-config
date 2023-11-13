{
  inputs,
  lib,
  ...
}: let
  mkDroidConfig = {
    extraModules ? [],
    homeManager_extraModules ? [],
    homeManager_sharedModules ? [],
    hostname ? "default",
    non-nixos ? false,
    system ? "aarch64-linux",
    stateVersion ? lib.fileContents ../.version,
    username ? null,
    ...
  }: {
    ${hostname} = inputs.nix-on-droid.lib.nixOnDroidConfiguration {
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.nix-on-droid.overlays.default
        ];
      };
      extraSpecialArgs = {inherit inputs username stateVersion;};
      home-manager-path = inputs.home-manager.outPath;
      modules =
        extraModules
        ++ lib.optional (hostname != null) ../profiles/hosts/${hostname}
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
              extraSpecialArgs = {inherit inputs non-nixos username stateVersion;};
              # useGlobalPkgs = true;
              useUserPackages = true;
              sharedModules = homeManager_sharedModules;
              config = {
                config,
                lib,
                pkgs,
                ...
              }: {
                imports =
                  lib.optional (username != null) ../profiles/users/${username}
                  ++ homeManager_extraModules;

                nixpkgs = {
                  config = {
                    allowUnfree = true;
                    # Workaround for https://github.com/nix-community/home-manager/issues/2942
                    allowUnfreePredicate = _: true;
                  };
                  # overlays = builtins.attrValues outputs.overlays;
                };

                home.stateVersion = "${stateVersion}";
                programs.home-manager.enable = true;
                manual.manpages.enable = false;
              };
            };
          })
        ];
    };
  };
in {
  flake.nixOnDroidConfigurations = lib.mkMerge [
    (mkDroidConfig {
      hostname = "oneplus5";
      username = "nix-on-droid";
      # system = "aarch64-linux";
      # extraModules = [];
      # homeManager_extraModules = [];
      homeManager_sharedModules = [
        # inputs.impermanence.nixosModules.home-manager.impermanence
        inputs.nix-doom-emacs.hmModule
        # inputs.nur.hmModules.nur
        # inputs.sops-nix.homeManagerModules.sops
      ];
    })
  ];
}
