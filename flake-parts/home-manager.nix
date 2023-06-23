{
  inputs,
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
    # buildSuites = profiles: f: lib.mapAttrs (_: lib.flatten) (lib.fix (f profiles));

    # homeModules = lib.buildModuleList ../modules/home-manager;
    # homeProfiles = lib.rakeLeaves ../profiles/home-manager;
    # homeSuites =
    #   buildSuites homeProfiles (profiles: suites: {
    #   });

    defaultModules = [
      # make flake inputs accessible in NixOS
      {
        _module.args.self = self;
        _module.args.inputs = inputs;
        _module.args.lib = lib;
      }
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
        extraSpecialArgs = {inherit username stateVersion;};
        modules =
          defaultModules
          ++ extraModules
          ++ [./home.nix];
      };
    };
  in {
    legacyPackages.homeConfigurations = lib.mkMerge [
      (mkHomeConfig {
        hostname = "d630";
        # username = "actoriu";
        extraModules = [];
      })
    ];
  };
}
