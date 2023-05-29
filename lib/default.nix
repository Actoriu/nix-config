{
  inputs,
  lib,
  self,
  ...
}:
lib.makeExtensible (self: let
  callLibs = file: import file { lib = self; };
in {
  flakeStateVersion = lib.fileContents ./.version;
  buildModuleList = callLibs ./build-module-list.nix {inherit self lib;};
  flattenTree = callLibs ./flatten-tree.nix {inherit lib;};
  rakeLeaves = callLibs ./rake-leaves.nix {inherit inputs lib;};
})
