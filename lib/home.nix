{
  inputs,
  lib,
  ...
}: let
  inherit (inputs) self;
  inherit (inputs.home-manager.lib) homeManagerConfiguration;
  inherit (outputs) version;
in {
  mkHomeConfig = {
    hostname ? null,
    username,
    system ? "x86_64-linux",
    sharedModules ? [
      {
        home = {
          inherit username;
          homeDirectory = "home/${username}";
          stateVersion = "${version}";
        };
        programs.home-manager.enable = true;
        manual.manpages.enable = false;
        systemd.user.startServices = "sd-switch";
      }
      ./modules/home-manager
    ],
    extraModules ? [],
    persistence ? false,
    ...
  }:
    homeManagerConfiguration {
      pkgs = self.legacyPackages."x86_64-linux";
      extraSpecialArgs = {
        inherit inputs self hostname username persistence;
      };
      modules = extraModules ++ sharedModules ++ [../users/${username}];
    };
}
