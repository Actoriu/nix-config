{
  self,
  home-manager,
  nix-on-droid,
  nixpkgs,
  templates,
  ...
}: let
  inherit (nixpkgs) lib;

  # genModules = hostName: {...}: {
  #   config,
  #   pkgs,
  #   ...
  # }: {
  #   imports = [(../hosts + "/${hostName}")];
  #   nix.registry = {
  #     nixpkgs.flake = nixpkgs;
  #     p.flake = nixpkgs;
  #     pkgs.flake = nixpkgs;
  #     templates.flake = templates;
  #   };

  #   home = {
  #     sessionVariables.NIX_PATH = lib.concatStringsSep ":" [
  #       "nixpkgs=${config.xdg.dataHome}/nixpkgs"
  #       "nixpkgs-overlays=${config.xdg.dataHome}/overlays"
  #     ];
  #   };

  #   xdg = {
  #     dataFile = {
  #       nixpkgs.source = nixpkgs;
  #       overlays.source = ../nix/overlays;
  #     };
  #     configFile."nix/nix.conf".text = ''
  #       flake-registry = ${config.xdg.configHome}/nix/registry.json
  #     '';
  #   };
  # };

  genConfiguration = hostName: {hostPlatform, ...}:
    nix-on-droid.lib.nixOnDroidConfiguration {
      pkgs = import nixpkgs {
        system = ${hostPlatform};
        overlays =
          # (builtins.attrValues self.overlays)
          ++ [
            nix-on-droid.overlays.default
          ];
      };
      # extraSpecialArgs = { inherit inputs self; };
      home-manager-path = home-manager.outPath;
      modules = [(../hosts + "/${hostName}")];
    };
in
  lib.mapAttrs genConfiguration (self.hosts.droid or {})
# {
#   lib,
#   inputs,
#   outputs,
#   version,
# }: let
#   inherit (inputs.nix-on-droid.lib) nixOnDroidConfiguration;
# in {
#   mkDroidConfig = {
#     devicename ? "default",
#     username ? null,
#     system ? "aarch64-linux",
#     add_extraModules ? [],
#     custom_extraModules ? [],
#     home_extraModules ? [],
#     sharedModules ? [
#       {
#         home-manager = {
#           useGlobalPkgs = true;
#           useUserPackages = true;
#           extraSpecialArgs = {inherit inputs;};
#           config = {...}: {
#             home.stateVersion = version;
#             manual.manpages.enable = false;
#             imports =
#               home_extraModules
#               ++ [
#                 ../modules/home-manager
#                 ../users/${username}
#               ];
#           };
#         };
#       }
#     ],
#     persistence ? false,
#     ...
#   }:
#     nixOnDroidConfiguration {
#       pkgs = import nixpkgs {
#         inherit system;
#         overlays = [
#           inputs.nix-on-droid.overlays.default
#         ];
#       };
#       extraSpecialArgs = {inherit inputs version persistence;};
#       home-manager-path = inputs.home-manager.outPath;
#       modules = add_extraModules ++ sharedModules ++ [../hosts/${devicename}];
#     };
# }

