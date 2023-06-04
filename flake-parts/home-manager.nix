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
        inputs.impermanence.nixosModules.home-manager.impermanence
        inputs.nur.hmModules.nur
        inputs.sops-nix.homeManagerModules.sops
      ];
    })
  ];

  mkHomeConfig = {
    extraModules ? [],
    hostname ? null,
    username ? null,
    system ? "x86_64-linux",
    ...
  }: {
    ${username} = inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = {};
      modules =
        defaultModules
        ++ extraModules
        # ++ lib.optional (username != null) ../users/${username}
        ++ [
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
        ];
    };
  };
in {
  flake.homeConfigurations = lib.mkMerge [
    # (mkHomeConfig {
    #   hostname = "d630";
    #   username = "actoriu";
    #   extraModules = [];
    # })
  ];
}
