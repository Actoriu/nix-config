{
  config,
  desktop,
  hostname,
  inputs,
  lib,
  outputs,
  pkgs,
  username,
  version,
  ...
}: {
  imports = [
    inputs.impermanence.nixosModules.home-manager.impermanence
    inputs.nur.hmModules.nur
    inputs.sops-nix.homeManagerModules.sops
    ../../modules/home-manager
    (../. + "/${username}")
  ];

  nixpkgs = {
    config = {
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
      allowBroken = true;
      allowUnsupportedSystem = true;
    };
    overlays = builtins.attrValues outputs.overlays;
  };

  home = {
    username = username;
    homeDirectory = "/home/${config.home.username}";
    stateVersion = "${version}";
  };

  programs.home-manager.enable = true;
  manual.manpages.enable = false;
  systemd.user.startServices = "sd-switch";
}
