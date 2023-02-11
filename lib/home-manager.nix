{
  self,
  home-manager,
  nixpkgs,
  templates,
  ...
}: let
  inherit (nixpkgs) lib;

  genModules = hostName: {homeDirectory, ...}: {
    config,
    pkgs,
    ...
  }: {
    imports = [(../hosts + "/${hostName}")];
    nix.registry = {
      nixpkgs.flake = nixpkgs;
      p.flake = nixpkgs;
      pkgs.flake = nixpkgs;
      templates.flake = templates;
    };

    home = {
      inherit homeDirectory;
      sessionVariables.NIX_PATH = lib.concatStringsSep ":" [
        "nixpkgs=${config.xdg.dataHome}/nixpkgs"
        "nixpkgs-overlays=${config.xdg.dataHome}/overlays"
      ];
    };

    xdg = {
      dataFile = {
        nixpkgs.source = nixpkgs;
        overlays.source = ../nix/overlays;
      };
      configFile."nix/nix.conf".text = ''
        flake-registry = ${config.xdg.configHome}/nix/registry.json
      '';
    };
  };

  genConfiguration = hostName: {hostPlatform, ...} @ attrs:
    home-manager.lib.homeManagerConfiguration {
      pkgs = self.pkgs.${hostPlatform};
      modules = [(genModules hostName attrs)];
      # extraSpecialArgs = {inherit inputs self;};
    };
in
  lib.mapAttrs genConfiguration (self.hosts.homeManager or {})
# {
#   lib,
#   inputs,
#   outputs,
#   version,
# }: let
#   inherit (inputs.home-manager.lib) homeManagerConfiguration;
# in {
#   mkHomeConfig = {
#     hostname ? null,
#     username,
#     system ? "x86_64-linux",
#     sharedModules ? [
#       {
#         home = {
#           inherit username;
#           homeDirectory = "home/${username}";
#           stateVersion = version;
#         };
#         programs.home-manager.enable = true;
#         manual.manpages.enable = false;
#         systemd.user.startServices = "sd-switch";
#       }
#       ./modules/home-manager
#     ],
#     extraModules ? [],
#     persistence ? false,
#     ...
#   }:
#     homeManagerConfiguration {
#       pkgs = self.legacyPackages."x86_64-linux";
#       extraSpecialArgs = {
#         inherit inputs hostname username persistence;
#       };
#       modules = extraModules ++ sharedModules ++ [../users/${username}];
#     };
# }

