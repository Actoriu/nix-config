{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [./cachix];

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    gc = {
      automatic = true;
      options = ''--delete-older-than 7d'';
    };

    optimise.automatic = true;

    settings = {
      allowed-users = ["@wheel"];
      auto-optimise-store = true;
      fallback = true;
      keep-outputs = true;
      keep-derivations = true;
      min-free = 1024 * 1024 * 1024;
      sandbox = true;
      system-features = ["nixos-test" "benchmark" "big-parallel" "kvm"];
      trusted-users = ["root" "@wheel"];
    };
  };
}
