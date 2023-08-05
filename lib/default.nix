{
  inputs,
  lib,
  pkgs,
  ...
} @ args:
with {inherit (lib) makeExtensible attrValues foldr;};
  (makeExtensible (self:
    with self;
      (import ./map.nix args).modules ./. (file: import file args)))
  .extend
  (self: super: foldr (x: y: x // y) {} (attrValues super))
