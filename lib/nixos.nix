{
  lib,
  inputs,
  outputs,
  ...
}: let
  inherit (inputs) self;
  inherit (inputs.nixpkgs.lib) nixosSystem;
  inherit (outputs) version;
in {
  mkNixosConfig = {
    hostname,
    username ? null,
    system ? "x86_64-linux",
    extraModules ? [],
    home_extraModules ? [],
    sharedModules ? [
      inputs.home-manager.nixosModules.home-manager
      {
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          extraSpecialArgs = {inherit inputs self persistence;};
          users.${username} = {...}: {
            home.stateVersion = "${version}";
            programs.home-manager.enable = true;
            manual.manpages.enable = false;
            systemd.user.startServices = "sd-switch";
            imports =
              home_extraModules
              ++ [
                ../modules/home-manager
                ../users/${username}
              ];
          };
        };
      }
      ../modules/nixos
    ],
    persistence ? false,
    ...
  }:
    nixosSystem {
      specialArgs = {
        inherit inputs self hostname username persistence;
      };
      modules = extraModules ++ sharedModules ++ [../hosts/${hostname}];
    };
}
