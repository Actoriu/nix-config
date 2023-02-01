{ lib }:

lib = makeExtensible (self: let
  callLibs = file: import file { lib = self; };
in
  rec {
    droid = callLibs ./droid.nix;
    home = callLibs ./home.nix;
    nixos = callLibs ./nixos.nix;
  })
