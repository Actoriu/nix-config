{
  lib,
  inputs,
  outputs,
  version
}: let
  inherit (inputs.nix-on-droid.lib) nixOnDroidConfiguration;
in {
  mkDroidConfig = {
    devicename ? "default",
    username ? null,
    system ? "aarch64-linux",
    add_extraModules ? [],
    custom_extraModules ? [],
    home_extraModules ? [],
    sharedModules ? [
      {
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          extraSpecialArgs = {inherit inputs;};
          config = {...}: {
            home.stateVersion = version;
            manual.manpages.enable = false;
            imports =
              home_extraModules
              ++ [
                ../modules/home-manager
                ../users/${username}
              ];
          };
        };
      }
    ],
    persistence ? false,
    ...
  }:
    nixOnDroidConfiguration {
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          inputs.nix-on-droid.overlays.default
        ];
      };
      extraSpecialArgs = {inherit inputs version persistence;};
      home-manager-path = inputs.home-manager.outPath;
      modules = add_extraModules ++ sharedModules ++ [../hosts/${devicename}];
    };
}
