{ inputs
, ...
}:

let
  inherit (inputs) self home-manager nix-on-droid nixpkgs deploy-rs;
  inherit (self) outputs;

  inherit (builtins) elemAt match any mapAttrs attrNames listToAttrs;
  inherit (nixpkgs.lib) nixosSystem filterAttrs genAttrs mapAttrs';
  inherit (home-manager.lib) homeManagerConfiguration;
  inherit (nix-on-droid.lib) nixOnDroidConfiguration;

  activate = type: config: deploy-rs.lib.${config.pkgs.system}.activate.${type} config;
in
rec {
  # Applies a function to a attrset's names, while keeping the values
  mapAttrNames = f:
    mapAttrs' (name: value: {
      name = f name;
      inherit value;
    });

  has = element: any (x: x == element);

  getUsername = string: elemAt (match "(.*)@(.*)" string) 0;
  getHostname = string: elemAt (match "(.*)@(.*)" string) 1;

  defaultSystems = [
    "aarch64-darwin"
    "aarch64-linux"
    "i686-linux"
    "x86_64-darwin"
    "x86_64-linux"
  ];

  eachDefaultSystem = genAttrs defaultSystems;

  mkSystem =
    { hostname
    , username ? null
    , pkgs
    , extraModules ? [ ]
    , extraSpecialArgs ? { }
    , sharedModules ? [
        home-manager.nixosModules.home-manager
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            extraSpecialArgs = extraSpecialArgs;
            users.${username} = { ... }: {
              home.stateVersion = "22.11";
              imports = [
                ../users/modules
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
      inherit pkgs;
      specialArgs = {
        inherit inputs outputs hostname username persistence;
      };
      modules = extraModules ++ sharedModules ++ [ ../hosts/${hostname} ];
    };

  mkHome =
    { hostname ? null
    , username
    , pkgs ? outputs.nixosConfigurations.${hostname}.pkgs
    , extraModules ? [ ]
    , sharedModules ? [
        {
          home = {
            inherit username;
            homeDirectory =
              if pkgs.stdenv.isDarwin
              then "/Users"
              else "/home" + "/${username}";
            stateVersion = "22.11";
          };
        }
      ]
    , persistence ? false
    , colorscheme ? null
    , ...
    }:
    homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = {
        inherit inputs outputs hostname username persistence colorscheme;
      };
      modules = extraModules ++ sharedModules ++ [ ../users/${username} ];
    };

  mkDroid =
    { devicename
    , pkgs
    , extraModules ? [ ]
    , persistence ? false
    ,
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
