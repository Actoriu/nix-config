{ config
, lib
, pkgs
, ...
}: {
  imports = [ ./cachix ];

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
      allowed-users = [ "@wheel" ];
      auto-optimise-store = true;
      fallback = true;
      keep-outputs = true;
      keep-derivations = true;
      min-free = 1024 * 1024 * 1024;
      sandbox = true;
      system-features = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
      trusted-users = [ "root" "@wheel" ];
    };

    # Add each flake input as a registry
    # To make nix3 commands consistent with the flake
    registry = lib.mapAttrs (_: value: { flake = value; }) inputs;

    # Map registries to channels
    # Very useful when using legacy commands
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;
  };
}
