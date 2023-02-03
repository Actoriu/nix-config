{
  self,
  lib,
  inputs,
} @ args:
with {inherit (lib) makeExtensible attrValues foldr;};
  (makeExtensible (final:
    with final;
      (import ./map.nix args).modules builtins.filterSource (path: type: !(type == "directory" && baseNameOf path == "compat")) ./. (file: import file args)))
  .extend
  (final: prev: foldr (x: y: x // y) {} (attrValues prev))
# {
#   lib,
#   inputs,
#   ...
# }:
# {
#   droid = import ./droid.nix {inherit lib inputs;};
#   home = import ./home.nix {inherit lib inputs;};
#   nixos = import ./nixos.nix {inherit lib inputs;};
# }

