{
  config,
  inputs,
  lib,
  pkgs,
  stateVersion,
  username,
  ...
}: {
  imports =
    [
      inputs.impermanence.nixosModules.home-manager.impermanence
      inputs.nur.hmModules.nur
      inputs.sops-nix.homeManagerModules.sops
    ]
    ++ lib.optional (username != null) ../users/${username};

  nixpkgs = {
    config = {
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
    # overlays = builtins.attrValues outputs.overlays;
  };

  home = {
    username = username;
    homeDirectory =
      (
        if pkgs.stdenv.isDarwin
        then "/Users"
        else "/home"
      )
      + "/${username}";
    stateVersion = stateVersion;
  };

  programs.home-manager.enable = true;
  manual.manpages.enable = false;
  systemd.user.startServices = "sd-switch";
}
