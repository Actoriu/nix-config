{
  inputs,
  lib,
  ...
}: let
  defaultModules = [
    ({...}: {
      imports = [inputs.home-manager.nixosModules.home-manager];
    })
  ];

  mkNixosConfig = {
    extraModules ? [],
    homeManager_extraModules ? [],
    hostname ? null,
    non-nixos ? false,
    system ? "x86_64-linux",
    stateVersion ? lib.fileContents ../.version,
    username ? null,
    ...
  }: {
    ${hostname} = inputs.nixpkgs.lib.nixosSystem {
      specialArgs = {
        inherit inputs username stateVersion;
      };
      modules =
        lib.optional (hostname != null) ../profiles/hosts/${hostname}
        ++ defaultModules
        ++ extraModules
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
                nvidia.acceptLicense = true;
              };
              hostPlatform = lib.mkDefault system;
            };

            networking.hostName = lib.mkDefault hostname;

            system.stateVersion = "${stateVersion}";

            home-manager = {
              extraSpecialArgs = {inherit inputs non-nixos username stateVersion;};
              # useGlobalPkgs = true;
              useUserPackages = true;
              users.${username} = {...}: {
                imports =
                  homeManager_extraModules
                  ++ [
                    ./home.nix
                  ];
              };
            };
          })
        ];
    };
  };
in {
  flake.nixosConfigurations = lib.mkMerge [
    (mkNixosConfig {
      hostname = "d630";
      username = "actoriu";
      extraModules = [
        inputs.impermanence.nixosModules.impermanence
        inputs.disko.nixosModules.disko
        # inputs.nur.nixosModules.nur
        inputs.sops-nix.nixosModules.sops
      ];
      homeManager_extraModules = [
        inputs.impermanence.nixosModules.home-manager.impermanence
        inputs.nix-doom-emacs.hmModule
        # inputs.nur.hmModules.nur
        inputs.sops-nix.homeManagerModules.sops
      ];
    })
  ];
}
