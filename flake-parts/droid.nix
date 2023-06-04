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
    defaultModules = [
      # make flake inputs accessible in NixOS
      {
        _module.args.self = self;
        _module.args.inputs = inputs;
        _module.args.lib = lib;
      }
      # load common modules
      ({...}: {
        imports = [
          inputs.impermanence.nixosModules.home-manager.impermanence
          inputs.nur.hmModules.nur
          inputs.sops-nix.homeManagerModules.sops
        ];
      })
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
                config = {...}: {
                  imports = [
                    ({
                      config,
                      lib,
                      ...
                    }: {
                      nixpkgs = {
                        config = {
                          allowUnfree = true;
                          # Workaround for https://github.com/nix-community/home-manager/issues/2942
                          allowUnfreePredicate = _: true;
                        };
                      };

                      home = {
                        username = username;
                        homeDirectory =
                          (
                            if pkgs.stdenv.isDarwin
                            then "/Users"
                            else "/home"
                          )
                          + "/${username}";
                        stateVersion = "${stateVersion}";
                      };

                      programs.home-manager.enable = true;
                      manual.manpages.enable = false;
                      systemd.user.startServices = "sd-switch";
                      imports =
                        defaultModules
                        ++ lib.optional (username != null) ../users/${username}
                        ++ [];
                    })
                  ];
                };
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
