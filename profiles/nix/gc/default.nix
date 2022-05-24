{
  pkgs,
  lib,
  ...
}: {
  # services.clean-gcroots.enable = true;
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = lib.mkDefault ''
      --delete-older-than 7d
    '';
  };
  # nix.settings = {
  #   min-free = 100 * 1024 * 1024;
  #   max-free = 100 * 1024 * 1024;
  # };
}
