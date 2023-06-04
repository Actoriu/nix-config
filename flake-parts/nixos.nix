{
  inputs,
  lib,
  stateVersion,
  self,
  ...
}: let
  mkNixosConfig = {
    extraModules ? [],
    hostname ? "",
    username ? "",
    system ? "x86_64-linux",
    ...
  }: {
    ${username} = lib.nixosSystem {
      specialArgs = {inherit username;};
      modules =
        extraModules
        # ++ lib.optional (hostname != null) ../hosts/nixos/${hostname}
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
              hostPlatform = lib.mkDefault system;
            };

            networking.hostName = lib.mkDefault hostname;

            system.stateVersion = stateVersion;
          })
        ];
    };
  };

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
        inputs.home-manager.nixosModules.home-manager
      ];
    })
  ];
in {
  flake.nixosConfigurations = lib.mkMerge [
    (mkNixosConfig {
      hostname = "d630";
      username = "actoriu";
      extraModules = defaultModules ++ [];
    })
  ];
}
