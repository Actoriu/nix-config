{ lib, inputs }:

# let
#   lib = lib.makeExtensible (self: let
#     callLibs = file: import file { lib = self; };
#   in
{
  droid = import ./droid.nix { inherit lib inputs; };
  home = import ./home.nix { inherit lib inputs; };
  nixos = import ./nixos.nix { inherit lib inputs; };
}
  # in lib
