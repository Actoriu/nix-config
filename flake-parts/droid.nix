{
  inputs,
  lib,
  stateVersion,
  ...
}: let
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
              config = {
                config,
                lib,
                pkgs,
                ...
              }: {
                nixpkgs = {
                  config = {
                    allowUnfree = true;
                    # Workaround for https://github.com/nix-community/home-manager/issues/2942
                    allowUnfreePredicate = _: true;
                  };
                  # overlays = builtins.attrValues outputs.overlays;
                };
                home.stateVersion = "${stateVersion}";
                manual.manpages.enable = false;
                programs.home-manager.enable = true;
                imports =
                  [
                    inputs.impermanence.nixosModules.home-manager.impermanence
                    inputs.nur.hmModules.nur
                    inputs.sops-nix.homeManagerModules.sops
                    ../modules/home-manager
                  ]
                  ++ lib.optional (username != null) ../users/${username};
              };
            };
          })
        ];
    };
  };
in {
  droidConfigurations = lib.mkMerge [
    (mkDroidConfig {
      # devicename = "oneplus5";
      extraModules = [];
    })
  ];
}
