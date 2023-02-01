{ lib }:

lib = makeExtensible (self: let
  callLibs = file: import file { lib = self; };
in
  {
    droid = callLibs ./droid.nix;
    home = callLibs ./home.nix;
    nixos = callLibs ./nixos.nix;
  })
