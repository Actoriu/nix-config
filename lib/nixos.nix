{
  self,
  home-manager,
  impermanence,
  nixos-hardware,
  nixpkgs,
  sops-nix,
  templates,
  ...
}: let
  inherit (nixpkgs) lib;

  nixRegistry = {
    nix.registry = {
      nixpkgs.flake = nixpkgs;
      p.flake = nixpkgs;
      pkgs.flake = nixpkgs;
      templates.flake = templates;
    };
  };

  genConfiguration = hostName: {
    address,
    hostPlatform,
    ...
  }:
    lib.nixosSystem {
      specialArgs = {
        impermanence = impermanence.nixosModules;
        nixos-hardware = nixos-hardware.nixosModules;
        hostAddress = address;
      };
      modules = [
        (../hosts + "/${hostName}")
        {
          nixpkgs.pkgs = self.pkgs.${hostPlatform};
          # FIXME: This shouldn't be needed, but is for some reason
          nixpkgs.hostPlatform = hostPlatform;
        }
        nixRegistry
        home-manager.nixosModules.home-manager
        impermanence.nixosModules.impermanence
        nixos-cn.nixosModules.nixos-cn-registries
        nixos-cn.nixosModules.nixos-cn
        sops-nix.nixosModules.sops
      ];
    };
in
  lib.mapAttrs genConfiguration (self.hosts.nixos or {})
# {
#   lib,
#   inputs,
#   outputs,
#   version,
# }: let
#   inherit (inputs.nixpkgs.lib) nixosSystem;
# in {
#   mkNixosConfig = {
#     hostname,
#       username ? null,
#       system ? "x86_64-linux",
#       extraModules ? [],
#       home_extraModules ? [],
#       sharedModules ? [
#         inputs.home-manager.nixosModules.home-manager
#         {
#           home-manager = {
#             useGlobalPkgs = true;
#             useUserPackages = true;
#             extraSpecialArgs = {inherit inputs persistence;};
#             users.${username} = {...}: {
#               home.stateVersion = version;
#               programs.home-manager.enable = true;
#               manual.manpages.enable = false;
#               systemd.user.startServices = "sd-switch";
#               imports =
#                 home_extraModules
#                 ++ [
#                   ../modules/home-manager
#                   ../users/${username}
#                 ];
#             };
#           };
#         }
#         ../modules/nixos
#       ],
#       persistence ? false,
#       ...
#   }:
#     nixosSystem {
#       specialArgs = {
#         inherit inputs hostname username version system persistence;
#       };
#       modules = extraModules ++ sharedModules ++ [../hosts/${hostname}];
#     };
# }

