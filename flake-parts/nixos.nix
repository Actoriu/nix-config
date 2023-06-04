{
  inputs,
  lib,
  pkgs,
  stateVersion,
  self,
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
        inputs.impermanence.nixosModules.impermanence
        inputs.disko.nixosModules.disko
        inputs.nur.nixosModules.nur
        inputs.sops-nix.nixosModules.sops
      ];
    })
  ];

  mkNixosConfig = {
    extraModules ? [],
    hostname ? null,
    username ? null,
    system ? "x86_64-linux",
    ...
  }: {
    ${hostname} = lib.nixosSystem {
      specialArgs = {inherit username stateVersion;};
      modules =
        defaultModules
        ++ extraModules
        ++ lib.optional (hostname != null) ../hosts/nixos/${hostname}
        ++ [
          inputs.home-manager.nixosModules.home-manager
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
              hostPlatform = lib.mkDefault system;
            };

            networking.hostName = lib.mkDefault hostname;

            system.stateVersion = "${stateVersion}";

            home-manager = {
              extraSpecialArgs = {inherit username stateVersion;};
              # useGlobalPkgs = true;
              useUserPackages = true;
              users.${username} = {...}: {
                imports =
                  [
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
                    })
                  ]
                  ++ [
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
                  ]
                  ++ lib.optional (username != null) ../users/${username};
              };
            };
          })
        ];
    };
  };
in {
  flake.nixosConfigurations = lib.mkMerge [
    (mkNixosConfig {
      # hostname = "d630";
      username = "actoriu";
      extraModules = [];
    })
  ];
}
