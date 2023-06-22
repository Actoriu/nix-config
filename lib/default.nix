{
  inputs,
  lib,
  ...
} @ args:
with {inherit (lib) makeExtensible attrValues foldr;};
  (makeExtensible (final:
    with final;
      (import ./map.nix args).modules ./. (file: import file args)))
  .extend
  (final: prev: foldr (x: y: x // y) {} (attrValues prev))
/*
lib.makeExtensible (self: let
  callLibs = file: import file ({inherit self;} // args);
in {
  importers = callLibs ./importers.nix;

  inherit (self.importers) buildModuleList flattenTree rakeLeaves;
};
*/

