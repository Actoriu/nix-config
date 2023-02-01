{ lib }:

# let
#   lib = lib.makeExtensible (self: let
#     callLibs = file: import file { lib = self; };
#   in
rec {
  droid = import ./droid.nix { inherit lib; };
  home = import ./home.nix { inherit lib; };
  nixos = import ./nixos.nix { inherit lib; };
}
  # in lib
