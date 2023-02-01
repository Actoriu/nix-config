{ lib }:

# let
#   lib = lib.makeExtensible (self: let
#     callLibs = file: import file { lib = self; };
#   in
rec {
  droid = import ./droid.nix;
  home = import ./home.nix;
  nixos = import ./nixos.nix;
}
  # in lib
