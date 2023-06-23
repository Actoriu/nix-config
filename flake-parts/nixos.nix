{
  config,
  inputs,
  lib,
  outputs,
  pkgs,
  self,
  stateVersion,
  ...
}: let
  # nixosModules = lib.buildModuleList ../modules/nixos;
  # nixosProfiles = lib.rakeLeaves ../profiles/nixos;
  # nixosSuites = ;
  nixosSystem = args:
    (lib.makeOverridable inputs.nixpkgs.lib.nixosSystem)
    (lib.recursiveUpdate args {
      # specialArgs = {
      #   profiles = nixosProfiles;
      #   suites = nixosSuites;
      #   users = nixosUsers;
      #   containers = nixosContainers;
      #   inherit self inputs homeProfiles homeSuites;
      # };
      modules =
        args.modules
        ++ [
          {
            networking.hostName = args.hostname;

            system.stateVersion = "${stateVersion}";

            nixpkgs = {
              config = {
                allowUnfree = true;
                # for build nvidia
                allowBroken = true;
              };
              hostPlatform = lib.mkDefault args.system;
              # overlays = builtins.attrValues outputs.overlays;
            };

            home-manager = {
              extraSpecialArgs = {
                inherit inputs outputs stateVersion;
                inherit (args) non-nixos username;
              };
              # useGlobalPkgs = true;
              useUserPackages = true;
              users.${args.username} = import ./home.nix;
            };
          }
        ];
    });

  defaultModules =
    # nixosModules ++
    [
      ({...}: {
        imports = [
          inputs.disko.nixosModules.disko
          inputs.home-manager.nixosModules.home-manager
          inputs.impermanence.nixosModules.impermanence
          inputs.nur.nixosModules.nur
          inputs.sops-nix.nixosModules.sops
        ];
      })
    ];
  /*
  mkNixosConfig = {
    extraModules ? [],
      hostname ? null,
      username ? null,
      system ? "x86_64-linux",
  }: {
    ${hostname} = inputs.nixpkgs.lib.nixosSystem {
      specialArgs = {
        inherit username stateVersion;
        # profiles = nixosProfiles;
        # suites = nixosSuites;
      };
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
            networking.hostName = hostname;

            system.stateVersion = "${stateVersion}";

            nixpkgs = {
              config = {
                allowUnfree = true;
                # for build nvidia
                allowBroken = true;
              };
              hostPlatform = lib.mkDefault system;
              # overlays = builtins.attrValues outputs.overlays;
            };

            home-manager = {
              extraSpecialArgs = {inherit inputs outputs username stateVersion;};
              # useGlobalPkgs = true;
              useUserPackages = true;
              users.${username} = import ./home.nix;
            };
          })
        ];
    };
    };
  */
in {
  d630 = nixosSystem {
    hostname = "d630";
    non-nixos = false;
    username = "actoriu";
    system = "x86_64-linux";
    modules = defaultModules ++ [];
  };
}
