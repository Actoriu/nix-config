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
  imports =
    [
      inputs.impermanence.nixosModules.home-manager.impermanence
      inputs.nur.hmModules.nur
      inputs.sops-nix.homeManagerModules.sops
      (../. + "/${username}")
    ]
    ++ (builtins.attrValues outputs.homeManagerModules);

  nixpkgs = {
    config = {
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
    overlays = builtins.attrValues outputs.overlays;
    # overlays = [
    #   # Add overlays your own flake exports (from overlays and pkgs dir):
    #   outputs.overlays.additions
    #   outputs.overlays.modifications
    #   outputs.overlays.spacemacs

    #   # You can also add overlays exported from other flakes:
    #   inputs.nixos-cn.overlay
    #   inputs.nur.overlay
    #   inputs.sops-nix.overlays.default
    # ];
  };

  home = {
    username = username;
    homeDirectory =
      if pkgs.stdenv.isDarwin
      then "/Users/${username}"
      else "/home/${username}";
    stateVersion = "${version}";
  };

  programs.home-manager.enable = true;
  manual.manpages.enable = false;
  systemd.user.startServices = "sd-switch";
}
