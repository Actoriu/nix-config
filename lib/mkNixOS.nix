{ inputs
, lib
, ...
}:

let
  inherit (inputs) self;
  inherit (inputs.nixpkgs.lib) nixosSystem;
in
{
  mkSystem =
    { hostname
    , username ? null
    , system ? "x86_64-linux"
    , extraModules ? [ ]
    , home_extraModules ? [ ]
    , sharedModules ? [
        inputs.home-manager.nixosModules.home-manager
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            extraSpecialArgs = { inherit inputs self persistence; };
            users.${username} = { ... }: {
              home.stateVersion = "22.11";
              programs.home-manager.enable = true;
              manual.manpages.enable = false;
              systemd.user.startServices = "sd-switch";
              imports = home_extraModules ++ [
                ../users/${username}
              ];
            };
          };
        }
      ]
    , persistence ? false
    , ...
    }:
    nixosSystem {
      inherit system;
      specialArgs = {
        inherit inputs self hostname username persistence;
      };
      modules = extraModules ++ sharedModules ++ [ ../hosts/${hostname} ];
    };
}
