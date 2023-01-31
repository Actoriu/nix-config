{ lib, ... }:

lib.makeExtensible (self:
  let
    callLibs = file: import file { lib = self; };
  in rec {
    droid = callLibs ./droid;
    home = callLibs ./home;
    nixos = callLibs ./nixos;
  })
