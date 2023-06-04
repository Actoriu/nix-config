{
  inputs,
  lib,
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
      specialArgs = {inherit username;};
      modules =
        defaultModules
        ++ extraModules
        # ++ lib.optional (hostname != null) ../hosts/nixos/${hostname}
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

            system.stateVersion = stateVersion;

            home-manager = {
              # extraSpecialArgs = {inherit username;};
              # useGlobalPkgs = true;
              useUserPackages = true;
              users.${username} = import ../users/shared;
            };
          })
        ];
    };
  };
in {
  flake.nixosConfigurations = lib.mkMerge [
    # (mkNixosConfig {
    #   hostname = "d630";
    #   username = "actoriu";
    #   extraModules = [];
    # })
  ];
}
