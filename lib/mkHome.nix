{ inputs
, lib
, ...
}:

let
  inherit (inputs) self;
  inherit (inputs.home-manager.lib) homeManagerConfiguration;
in
{
  mkHome =
    { hostname ? null
    , username
    , system ? "x86_64-linux"
    , pkgs
    , extraModules ? [ ]
    , sharedModules ? [
        {
          home = {
            inherit username;
            homeDirectory = "home/${username}";
            stateVersion = "22.11";
          };
          programs.home-manager.enable = true;
          manual.manpages.enable = false;
          systemd.user.startServices = "sd-switch";
        }
      ]
    , persistence ? false
    , ...
    }:
    homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = {
        inherit inputs self hostname username persistence;
      };
      modules = extraModules ++ sharedModules ++ [ ../users/${username} ];
    };
}
