{ inputs
, ...
}:

let
  inherit (inputs) self home-manager nix-on-droid nixpkgs deploy-rs;
  inherit (self) outputs;

  inherit (builtins) elemAt match any mapAttrs attrValues attrNames listToAttrs;
  inherit (nixpkgs.lib) nixosSystem filterAttrs genAttrs mapAttrs';
  inherit (nixpkgs.pkgs) stdenv;
  inherit (home-manager.lib) homeManagerConfiguration;
  inherit (nix-on-droid.lib) nixOnDroidConfiguration;

  activate = type: config: deploy-rs.lib.${config.pkgs.system}.activate.${type} config;
in
rec {
  # Applies a function to a attrset's names, while keeping the values
  mapAttrNames = f: mapAttrs' (name: value: { name = f name; inherit value; });

  has = element: any (x: x == element);

  getUsername = string: elemAt (match "(.*)@(.*)" string) 0;
  getHostname = string: elemAt (match "(.*)@(.*)" string) 1;

  systems = [
    "aarch64-darwin"
    "aarch64-linux"
    "i686-linux"
    "x86_64-darwin"
    "x86_64-linux"
  ];
  forAllSystems = genAttrs systems;

  homePrefix = if stdenv.isDarwin then "/Users" else "/home";

  mkSystem =
    { hostname
    , username ? null
    , pkgs
    , hardwareModules ? [ ]
    , baseModules ? [
        home-manager.nixosModules.home-manager
        ../modules/nixos
      ]
    , extraModules ? [ ]
    , persistence ? false
    }:
    nixosSystem {
      inherit pkgs;
      specialArgs = {
        inherit inputs outputs hostname username persistence;
      };
      modules = baseModules ++ hardwareModules ++ extraModules ++ [ ../hosts/${hostname} ];
    };

  mkHome =
    { username
    , hostname ? null
    , pkgs ? outputs.nixosConfigurations.${hostname}.pkgs
    , baseModules ? [
        ../modules/users
        {
          home = {
            inherit username;
            homeDirectory = "${homePrefix}/${username}";
            stateVersion = "22.11";
          };
        }
      ]
    , extraModules ? [ ]
    , persistence ? false
    , colorscheme ? null
    , wallpaper ? null
    , features ? [ ]
    }:
    homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = {
        inherit inputs outputs hostname username persistence
          colorscheme wallpaper features;
      };
      modules = baseModules ++ extraModules ++ [ ../users/${username} ];
    };

  mkDroid =
    { devicename
    , pkgs
    , extraModules ? [ ]
    , persistence ? false
    }:
    nixOnDroidConfiguration {
      inherit pkgs;
      extraModules = extraModules;
      extraSpecialArgs = {
        inherit inputs outputs devicename persistence;
      };
      config = ../hosts/${devicename};
    };

  mkDeploys = nixosConfigs: homeConfigs:
    let
      nixosProfiles = mapAttrs mkNixosDeployProfile nixosConfigs;
      homeProfiles = mapAttrs mkHomeDeployProfile homeConfigs;
      hostnames = attrNames nixosProfiles;

      homesOn = hostname: filterAttrs (name: _: (getHostname name) == hostname) homeProfiles;
      systemOn = hostname: { system = nixosProfiles.${hostname}; };
      profilesOn = hostname: (systemOn hostname) // (mapAttrNames getUsername (homesOn hostname));
    in
    listToAttrs (map
      (hostname: {
        name = hostname;
        value = {
          inherit hostname;
          profiles = profilesOn hostname;
        };
      })
      hostnames);

  mkNixosDeployProfile = _name: config: {
    user = "root";
    path = activate "nixos" config;
  };

  mkHomeDeployProfile = name: config: {
    user = getUsername name;
    path = activate "home-manager" config;
  };
}
