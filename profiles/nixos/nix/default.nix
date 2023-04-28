{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [./cachix];

  nix = {
    /*
    gc = {
      automatic = true;
      options = ''--delete-older-than 7d'';
    };

    optimise.automatic = true;
    */

    settings = {
      allowed-users = ["@wheel"];
      auto-optimise-store = true;
      experimental-features = ["nix-command" "flakes"];
      fallback = true;
      keep-outputs = true;
      keep-derivations = true;
      sandbox = true;
      system-features = ["nixos-test" "benchmark" "big-parallel" "kvm"];
      trusted-users = ["root" "@wheel"];
    };
  };
}
