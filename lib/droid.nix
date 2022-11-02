{ inputs
, lib
, self
, ...
}:

let
  inherit (inputs) self;
  inherit (inputs.nix-on-droid.lib) nixOnDroidConfiguration;
in
{
  mkDroid =
    { devicename
    , username ? null
    , system ? "aarch64-linux"
    , add_extraModules ? [ ]
    , custom_extraModules ? [ ]
    , home_extraModules ? [ ]
    , sharedModules ? [
        {
          home-manager = {
            useUserPackages = true;
            extraSpecialArgs = { inherit inputs self persistence; };
            config = { ... }: {
              home.stateVersion = "22.11";
              manual.manpages.enable = false;
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
    nixOnDroidConfiguration {
      inherit system;
      extraSpecialArgs = { inherit inputs self persistence; };
      extraModules = custom_extraModules;
      config = { ... }: {
        imports = add_extraModules ++ sharedModules ++ [ ../hosts/${devicename} ];
      };
    };
}
