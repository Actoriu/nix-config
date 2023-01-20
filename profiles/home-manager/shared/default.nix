{ inputs
, outputs
, ...
}:

{
  imports = [
    inputs.impermanence.nixosModules.home-manager.impermanence
  ];

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = true;
      allowUnsupportedSystem = true;
    };
    overlays = builtins.attrValues outputs.overlays;
  };

  programs.home-manager.enable = true;
  home.stateVersion = "22.11";
  # manual.manpages.enable = false;
  systemd.user.startServices = "sd-switch";

}
