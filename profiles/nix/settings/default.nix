{ ... }:

{
  nix = {
    # localRegistry = {
    #   enable = false;
    #   cacheGlobalRegistry = true;
    #   noGlobalRegistry = false;
    # };
    settings = {
      auto-optimise-store = true;
      allowed-users = [ "@wheel" ];
      trusted-users = [ "root" "@wheel" ];
      keep-outputs = true;
      keep-derivations = true;
      fallback = true;
      sandbox = true;
      system-features = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
    };
    optimise.automatic = true;
  };
}
